; Memory Map after stage 1 runs:
; 0x0000 ---------------------
;        |   IVT + BDA (BIOS)|
; 0x0500 ---------------------
;        |   Free            |
; 0x7C00 --------------------- <- Stack grows down from here
;        |  Stage 1 (MBR)    |  512 bytes
; 0x7E00 --------------------- <- Stage 2 loaded here
;        |  Stage 2 code     |  16 KB (32 sectors)
; 0xFE00 ---------------------
;        |  Free             |
;        ---------------------


; MBR bootstrap for OS
; MBR is trhe master boot record, it's the very first sector (512 bytes) of a disk. BIOS reads it, checks for a signature at 0x55AA, and if found, executes it as code. It's the first thing that runs after the BIOS finishes its hardware checks
; Bios load this to 0x7C00 in 16-bit real mode
; Job of stage 1: Load stage 2 from sectors 2..33 into 0x7E00, then jump to it

; Tell NASM to generate 16-bit machine code. BIOS starts the cpu in real mode, which is 16-bit. Without this, NASM defaults to 16-bit for bin format anyway, but being explicit is good practice.
; In real mode, registers are 16-bit (AX not EAX), and addresses use the segment:offset model.
[BITS 16]

; Tell NASM "this code will be loaded at address 0x7C00." This doesn't actually place it there, the BIOS will handle that. But NASM needs to know so it calculates label addresses correctly.
; I think it's like if err_msg is 80 bytes into the file, NASM needs to emit 0x7C00 + 80 = 0x7C50 when you do mov si, err_msg, not just 80
; 0x7C00 is hardcoded in every IBM-compatible BIOS since 1981. When the BIOS finishes POST, it reads sector 1 of the boot disk into physical addr 0x7C00 and jumps there. We don't get a choice
[ORG 0x7C00]

; The MBR occupies 512 bytes (0x7C00 - 0x7DFF). The byte immediately after is 0x7E00. Loading stage 2 here wastes no gap, and the region from 0x7E00 up to ~0x80000 is free conventional memory that BIOS guarantees is usable.
STAGE2_ADDR     equ 0x7E00
; 32 sectors x 512 bytes/sector = 16,384 bytes (16KB). Stage 2 will need to set up A20, enter protected mode, load the kernel etc. 16KB is generous for that. We could use less, but sectors are cheap and this avoids having to change it later.
; It also stays well within the free memory region (16KB ends at 0xBE00, far from any dangerous areas)
STAGE2_SECTORS  equ 32
; Industry convention for retries, 3 attempts handles transient errors without hanging forever
MAX_RETRIES     equ 3


; Byte 0: jump over BPB to bootcode
; every FAT boot sector starts with a short jump + NOP. the BIOS and
; filesystem tools both expect this signature. without it, some BIOSes
; won't recognize the disk as bootable and mkfs wont see it as FAT
jmp short bpb_end
nop

; Bytes 3-61: BIOS parameter block (BPB)
; these fields describe the FAT16 filesystem geometry. mkfs.fat will
; overwrite them with correct values when we format the disk
; the placeholders here are just so the binary has the right layout

; stage2 reads these at runtime via INT 0x13 to find the FAT table, root dir, and data region on the disk
OEMName: db "MYBOOT  " ; 8 bytes (offset 3-10)
BytesPerSec: dw 512 ; 2 bytes (offset 0x0B)
SecPerClus: db 4 ; 1 byte (offset 0x0D)
ReservedSecs: dw 33 ; 2 bytes (offset 0x0E) boot sector + 32 stage2
NumFATs: db 2 ; 1 byte (offset 0x10)
RootEntries: dw 512 ; 2 bytes (offset 0x11)
TotalSecs16: dw 0 ; 2 bytes (offset 0x13) (if zero, use TotalSecs32)
MediaType: db 0xF8 ; 1 byte (offset 0x15) 0x0F8 = hard disk
FATSize16: dw 0 ; 2 bytes (offset 0x16) filled by mkfs
SecPerTrack: dw 63 ; 2 bytes (offset 0x18)
NumHeads: dw 16 ; 2 bytes (offset 0x1A)
HiddenSecs: dd 0 ; 4 bytes (offset 0x1C)
TotalSecs32: dd 32768 ; 4 bytes (offset 0x20) = 16MB
; Extended BPB (FAT12/16 specific)
BS_DrvNum: db 0x80 ; 1 byte (offset 0x24)
BS_Reserved1: db 0 ; 1 byte (offset 0x25)
BS_BootSig: db 0x29 ; 1 byte (offset 0x26) 0x29 = extended fields present
BS_VolID: dd 0x12345678 ; 4 bytes (offset 0x27)
BS_VolLabel: db "TUPLEOS    " ; 11 bytes (offset 0x2B) space-padded
BS_FSType: db "FAT16   " ; 8 bytes (offset 0x36) space-padded

; Byte 62: Boot code starts here
bpb_end:
    mov [boot_drive], dl

start:
    ; Zero all segment registers, flat real mode at segment 0
    ; XOR of a register with itself always produces zero. Standard x86 idiom for zeroing a register, it's 2 bytes vs 3 bytes for mov ax, 0. We need segment 0 so that segment:offset addressing gives us flat access to the first 64KB
    xor ax, ax
    ; Set data segment and extra segment to 0. In real mode, every memory access uses segment * 16 + offset.
    ; We can't mov ds, 0 directly because x86 doesn't allow loading an immediate value into a segment register. You have to go through a general-purpose register
    mov ds, ax
    mov es, ax

    ; Stack grows downward from 0x7C00 (just below the MBR)
    mov ss, ax              ; set stack segment to 0
    mov sp, 0x7C00          ; set stack pointer to beginning of free conventional memory (see map at bottom)

    ; Clear direction flag, String operations like lodsb can go forward (SI increments) or backward (SI decrements). cld ensures forward, Some BIOSes leave DF set, which would make our print routine read backwards through memory. One byte, prevents a subtle bug gg ;)
    cld


    ; Load stage 2: INT 0x13 AH=02h (CHS read)
    ; Sectors 2-33, Cyliner 0, Head 0, all within one track

    ; Use DI as our retry counter. We need, AX,BX,CX,DX for the INT 0x13 params, and SI for print_string. DI is the only general purpose register left
    mov di, MAX_RETRIES

; local label, in NASM, .retry is scoped under the last non-local label (start). This avoids name collisions, you can have .retry in multiple functions
.retry:
    ; BIOS: read disk sectors. AH is a BIOS function number. INT 0x13, AH=02h = "Read Sectors From Drive." The BIOS interrupt system uses AH as a function selector, different values do different things (read,write,reset,get params, etc.)
    mov ah, 0x02
    ; How many sectors? AL is the number of sectors to read (32). AH and AL are the upper and lower bytes of AX. So after these two instructions, AX = 0x0220 (function 02h, read 32 sectors)
    mov al, STAGE2_SECTORS

    ; CHS addressing: Sectors are 1-indexed in CHS, sector 1 is the MBR itself, so stage 2 starts at sector 2. We write stage 2 immediately after the MBR on disk. On any drive geometry, the first 63 sectors are all on cylinder 0, head 0, so 32 sectors starting at sector 2 (ending at sector 33) fits easily.
    mov ch, 0               ; cylinder 0
    mov cl, 2               ; Start at sector 2 (sector 1 = MBR)
    mov dh, 0               ; head 0. First head, first cylinder. We're reading from the very beginning of the disk
    mov dl, [boot_drive]    ; We reload the drive number from memory because previous failed attempts may have nuked DL
    mov bx, STAGE2_ADDR     ; ES:BX = 0x0000:0x7E00
    ; Software interrupt. transfers control to the BIOS disk service routine at interrupt vector 0x13. The bios reads the params from all the registers we just set up, performs the disk read, then returns
    ; On return: CF (carry flag) = 0 means success, 1 means failure. AL = number of sectors actually read
    int 0x13
    ; Jump if not carry. If CF=0, the read succeeded, jump to .read_ok. If CF=1, fall through the retry logic
    jnc .read_ok

    ; Failed, reset disk controller and retry
    dec di
    jz .error
    xor ah, ah              ; AH=00h: reset disk
    mov dl, [boot_drive]
    int 0x13
    jmp .retry

.read_ok:
    ; verify BIOS read the correct number of sectors
    cmp al, STAGE2_SECTORS
    jne .error

    ; Hand off to stage 2, pass boot drive in DL
    mov dl, [boot_drive]
    jmp 0x0000:STAGE2_ADDR

.error:
    mov si, err_msg
.print:
    lodsb
    test al, al
    jz .halt
    mov ah, 0x0E            ; BIOS teletype output
    xor bh, bh
    int 0x10
    jmp .print
.halt:
    cli
    hlt
    jmp .halt               ; catch NMIs


; DATA

err_msg:    db "Stage1: disk error", 0
boot_drive: db 0

; Pad to 510 bytes + boot sig
times 510 - ($ - $$) db 0
dw 0xAA55
