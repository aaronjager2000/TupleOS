; In stage 1, our MBR is stuck in 512 bytes. It can do exactly 1 thing: load more code from disk and jump to it. Stage 2 is that "more code"
; it lives at 0x7E00 (the byte right after the MBR) and has 16KB to work with. For this step, stage 2 just needs to prove it's alive: set up its environment, clear the screen, and print a banner.

; 1. Save the boot drive
; DL contains the boot drive number passed from stage 1. We save it to memory immediately because the very next instructions will nuke the register state. We'll need this drive number later in future steps when stage 2 reads the kernel from disk

; 2. Set up segment registers (DS, ES, SS)
; Stage 2 is assembled with [ORG 0x7E00], meaning NASM calculates all label addresses relative to 0x7E00. For those addresses to be correct at runtime, the segment registers must be 0x0000 so that segment * 16 + offset = 0 + offset = offset.
; We set all data segments to zero, exactly like stage 1 did for itself
; You can't mov ds, 0 directly on x86, segment registers can only be loaded from general-purpose register, so we go through AX.

; 3. Set up a new stack
; Stage 1's stack sat right below 0x7C00, growing downward into low memory. That was fine for stage 1, but now stage 2 occupies 0x7E00 - 0xFE00. We need a stack that won't collide with either stage 1 or stage 2's code.
; I'll place the stack at 0x9000 (SS=0x0000, SP=0x9000). The stack grows downward from 0x9000 toward 0x8000, giving us 4KB of stack space. This sits above stage 2's end (0xFE00? wait as I'm typing this I realize this is wrong)

; mov ss, 0
; move sp, 0x7C00
; 0x7BFE 

; 0x7000

; okay scratch that bullshit. Stage 2 occupies 0x7E00 to 0x7E00 + 16KB = 0xBE00. So we should place the stack ABOVE stage 2. A good choice is 0xFFFF or just use a round address like 0x0000:0x7C00 (reuse stage 1's old stack area below the MBR)
; since stage 1 is done executing it's stack space is free. But the cleanest approach is to put the stack well above stage 2 at something like 0x9C00.. actualyl fuck eosijjioeawjeoiaweawjofwfeoijfwe

; okay lock in FN
; 0x7C00 - 0x7DFF = Stage 1 (512 bytes, still in memory but done executing)
; 0x7E00 - 0xBDFF = Stage 2 (16KB)
; 0xBE00+ = Free conventional memory

; okay so placing the stack top at 0xFFFF (still segment 0) with SS=0, SP=0xFFFF gives us ~16KB of stack from 0xBE00 to 0xFFFF. But conventionally you would use rounder numbers, I'll use SS=0x0000, SP=0x7C00, the same spot stage 1 used.
; Stage 1's code is still at 0x7C00-0x7DFF but we'll never execute it again, and the stack grows DOWNWARD from 0x7C00 into the free area 0x0500-0x7BFF (~30KB of stack). This is the simplest and most conventional choice imo.

; 4. Clear the screen (INT 0x10, AH=0x00)
; BIOS interrupt 0x10 with AH=0x00 sets the video mode. Mode 0x03 is the standard 80x25 color text mode (what we use). Setting the video mode also clears the screen as a side effect, the BIOS zeroes the entire text buffer at 0xB8000. This gives us a fresh canvas

; 5. Print the banner (print_string routine)
; The print_string routine is a loop:
;  1: Load a byte from [SI] into AL using lodsb (which also increments SI)
;  2: If it's zero (null terminator), stop
;  3: Otherwise, call INT 0x10 with AH=0x0E (teletype output, see stage 1 for more details). This prints one char at the current cursor position and advances the cursor
;  4: Repeat

; BH=0 selects page 0 (the active display page), and BL=0x07 gives us light gray on black (the default text attribute). On BIOSes BL BL is ignored for text modes but setting it is good practice.

; 6. print_hex routine
; print_hex takes a 16-bit value in AX and prints it as 0x1234. It:
;  1: Prints the literal prefix "0x"
;  2: Extracts each nibble from the top down using shifts and masks
;  3: Converts each niblle to its ASCII hex char ('0'-'9' or 'A'-'F')
;  4: Prints each char via INT 0x10

; The conversion works by: take a nibble value 0-15, add '0' (0x30). If the result is > '9' (meaning the original nibble was 10-15 / A-F), add 7 more to jump from the ASCII '9'+1 range up to 'A'

; 7. Halt
; After printing, we halt with cli (disable interrupts) + hlt, the jmp after hlt catches NMIs (non-maskable interrupts that ignore cli) so the CPU just halts again rather than executing whatever garbage comes next in memory.

[BITS 16]
[ORG 0x7E00]

; memory map buffer lives at 0x6000. we originally had this at 0x8000 but that's inside stage 2's
; code+data region (stage 2 starts at 0x7E00 and our binary is ~1KB, extending past 0x8000).
; writing E820 entries there was overwriting our own string data. 0x6000 is safely in free low
; memory, well above the BIOS data area (ends at 0x0500) and well below the stack (0x7C00).
; layout: [dword entry_count] followed by N entries of 24 bytes each
MMAP_ADDR       equ 0x6000
MMAP_ENTRIES    equ 0x6000          ; dword: how many entries we collected
MMAP_DATA       equ 0x6004          ; entries start right after the count
MMAP_ENTRY_SIZE equ 24              ; 8 base + 8 length + 4 type + 4 ACPI extended attrs

; FAT16 filesystem
SECTOR_BUF equ 0x0500 ; 512-byte scratch buffer (free memory above BDA)
FAT_BUF equ 0x1000 ; FAT table cache (up to 16KB 0x1000-0x4FFF)
KERNEL_SEG equ 0x1000 ; kernel load segment -> physical address 0x10000 (64KB mark)
KERNEL_OFF equ 0x0000 ; offset within segment, we load the kernel starting at the beginning of the segment

stage2_start:
    ; 1. Save boot drive (DL will be nuked by segment setup)
    mov [boot_drive], dl

    ; 2. Set up segment registers, flat real mode at segment 0
    xor ax, ax
    mov ds, ax
    mov es, ax

    ; 3. Set up stack, stage 1 code at 0x7C00 is dead now. Reuse the region below it:
    ; stack grows down from 0x7C00 into 0x0500-0x7BFF (~30KB free.)
    mov ss, ax
    mov sp, 0x7C00

    ; Forward string operations (safety against BIOSes that leave DF set)
    cld

    ; 4. Set video mode
    mov ah, 0x00
    mov al, 0x03
    int 0x10

    ; 5. Print welcome banner
    mov si, banner_top
    call print_string

    mov si, banner_title
    call print_string

    mov si, banner_bottom
    call print_string

    ; Print boot drive info as proof of life
    mov si, msg_drive
    call print_string

    xor ax, ax ; zero-extend boot drive byte into AX
    mov al, [boot_drive]
    call print_hex

    mov si, msg_crlf
    call print_string

    mov si, msg_ok
    call print_string

    call enable_a20

    ; verify ts worked
    call check_a20
    test ax, ax
    jnz .a20_good

    ; All 3 methods failed
    mov si, msg_a20_fail
    call print_string
    jmp .halt

.a20_good:
    mov si, msg_a20_ok
    call print_string

    ; 7. Detect memory map, we need to know what physical memory is actually usable
    ; before we leave real mode. this replaces what GRUB gave us via multiboot.
    ; E820 returns one entry per call in a loop, each entry describes a region of
    ; physical memory (base, length, type). we stash the results at 0x8000 so the
    ; kernel can read them later
    call detect_memory

    ; dump the map to screen so we can see what the BIOS gave us
    mov si, msg_mmap_hdr
    call print_string
    call print_memory_map

    ; 8. Initialize FAT16 filesystem
    call fat16_init

    ; 9. Find and load kernel from FAT16 partition
    call fat16_find_file
    test ax, ax ; AX = 0 means file not found
    jz .no_kernel

    ; AX = first cluster of TUPLEOS.bin
    call fat16_load_file
    jmp .kernel_ready

.no_kernel:
    ; can't boot without a kernel, halt here
    jmp .halt

.kernel_ready:
    ; GDT + protected mode switch
    ; This must come AFTER every BIOS call (A20, E820, future disk reads)



;  GDT + protected mode switch
;  This must come AFTER every BIOS call (A20, E820, future disk reads)
;  this is because BIOS interrupts only work in real mode. Once CR0.PE is set,
;  INT 0x10/0x13/0x15 are all gonna die

;  The switch sequence:
;   1. cli  Disable interrupts (an interrupt between setting CR0.PE and loading a protected-mode IDT = triple fault = reboot) real mode IVT is invalid in PM
;   2. lgdt   tell the cpu where our global descriptor table lives
;   3. set CR0.PE    flip the protection-enable bit, now we are in PM
;   4. far jump    load CS with a 32-bit code selector and flush the prefetch pipeline

; print a status message while we still have BIOS teletype output
; after the switch, INT 0x10 will be dead and we can only write to our VGA memory directly
; this is the last thing we'll ever print via BIOS

mov si, msg_pm
call print_string

; disable all maskable interrupts. because in real mode, the CPU uses the IVT (interrupt vector table) at adress 0x0000, which is a table of 256 4-byte real-mode far pointers (segment:offset)
; in protected mode the CPU expects an IDT (interrupt descriptor table) with 8-byte entries in a completely different format as well
; if we have a hardware interrupt (keyboard, timer) fires after we set CR0.PE but before we load a protected-mode IDT, the CPu will try to interpret those 4-byte IVT entries as 8-byte IDT gates and it will read garbage, it will fault, fault again (double fault), and then triple fault
; and then reboot gg no re. cli prevents that by masking the interrupts until the kernel sets up its own IDT and re-enables them with sti.
cli

; next, we load the GDT register. LGDT reads a 6-byte structure from memory:
; bytes 0-1: size of the GDT in bytes, minus 1 (16-bit)
; bytes 2-5: linear physical addr of the GDT (32-bit)
; after this, the CPU knows where our GDT is and how big it is, but we're still in real mode.
; LGDT alone doesn't trigger the switch, it just prepares the pointer so the CPU can look up segment descriptors when we actually need them
lgdt [gdt_descriptor]

; then we set the PE bit in CR0. CR0 is a control register that governs fundamental CPU modes. bit 0 is PE. we can't just "mov cr0, 1" because other bits in CR0 matter too (like the paging bit at bit 31)
; so we read-modify-write: read current val, OR in bit 0, write it back
; the instant this MOV executes, the CPU is in protected mode. every memory access now goes through the GDT for segment validation
mov eax, cr0
or eax, 0x00000001
mov cr0, eax

; last step. far jump into 32-bit code. this single instruction needs to do 3 thingsL
;  1. loads CS with 0x08, in protected mode, CS doesn't hold a base address anymore, it holds a selector, which is a byte offset into the GDT
;   0x08 means "the entry at byte 8 in the GDT", which is gdt_code, our 32-bit kernel code segment.
;   the cpu reads that descriptor, checks it out, makes sure it's present, makes sure it's executable, and that we have perms/the right privilege, then loads its attributes into a hidden part of CS
;   from now on, every instruction fetch is validated against this descriptor

;  2. flush the CPU prefetch pipeline. the cpu fetches and decodes instructions ahead of execution. the pipeline may contain the bytes after this JMP already decoded as 16-bit real mode instructions.
;    BUT we are in 32-bit mode now, those decodings will be wrong. the far jump will force the CPU to throw away everything in the pipeline and re-fetch from pm_entry, this time it will decode as 32-bit instructions

;  3. jumps to pm_entry, where [BITS 32] will tell NASM to emit 32-bit opcodes.

jmp 0x08:pm_entry



.halt:
    cli
    hlt
    jmp .halt

; print_string Print null-terminated string at DS:SI
; nukes: AX, BX, SI

print_string:
    lodsb  ; AL = [DS:SI], SI++
    test al, al  ; null terminatior?
    jz .done
    mov ah, 0x0E  ; BIOS teletype output
    mov bh, 0x00  ; page 0
    mov bl, 0x07  ; light gray on black
    int 0x10
    jmp print_string
.done:
    ret

; print_hex Print 16-bit value in AX as "0xNNNN"
; Nukes: AX, BX, CX, DX

print_hex:
    mov cx, ax ; save the value in CX

    ; print "0x" prefix
    mov al, '0'
    mov ah, 0x0E
    xor bh, bh
    int 0x10
    mov al, 'x'
    int 0x10

    ; print 4 hex digits, high nibble first
    ; CX holds the value, we rotate it left to bring each nibble to the top
    mov dx, 4 ; 4 nibbles to print
.hex_loop:
    rol cx, 4 ; rotate left 4, top nibble moves to bottom
    mov al, cl
    and al, 0x0F ; isolate low nibble
    add al, '0'  ; convert to ASCII '0' - '9'
    cmp al, '9'
    jbe .hex_print
    add al, 7  ; adjust 0x3A-0x3F -> 'A' - 'F'
.hex_print:
    mov ah, 0x0E
    int 0x10
    dec dx
    jnz .hex_loop
    ret

; print_hex_raw, same as print_hex but without the "0x" prefix
; used when we need to print two 16-bit halves back to back as one 32-bit number
; (print_hex would give us "0xHHHH0xLLLL" which looks dumb)
; nukes: AX, BX, CX, DX

print_hex_raw:
    mov cx, ax
    mov dx, 4
.hex_loop:
    rol cx, 4
    mov al, cl
    and al, 0x0F
    add al, '0'
    cmp al, '9'
    jbe .hex_print
    add al, 7
.hex_print:
    mov ah, 0x0E
    xor bh, bh
    int 0x10
    dec dx
    jnz .hex_loop
    ret

enable_a20:
    ; method 1: BIOS INT 0x15
    mov si, msg_a20_bios
    call print_string

    mov ax, 0x2401
    int 0x15
    jc .try_kbd

    call check_a20
    test ax, ax
    jnz .a20_done

.try_kbd:
    ; method 2: keyboard controller
    mov si, msg_a20_kbd
    call print_string

    call kbd_wait
    mov al, 0xD1
    out 0x64, al

    call kbd_wait
    mov al, 0xDF
    out 0x60, al

    call kbd_wait

    mov cx, 64
.kbd_delay:
    in al, 0x64
    loop .kbd_delay

    call check_a20
    test ax, ax
    jnz .a20_done

.try_fast:
    ; method 3: Fast A20 (port 0x92)
    mov si, msg_a20_fast
    call print_string

    in al, 0x92
    or al, 0x02
    and al, 0xFE
    out 0x92, al

    call check_a20
    test ax, ax
    jnz .a20_done

.a20_done:
    ret

check_a20:
    pushf
    push ds
    push es
    push di
    push si

    xor ax, ax
    mov ds, ax
    mov si, 0x0500

    mov ax, 0xFFFF
    mov es, ax
    mov di, 0x0510

    mov al, [ds:si]
    push ax
    mov al, [es:di]
    push ax

    mov byte [ds:si], 0x00
    mov byte [es:di], 0xFF

    cmp byte [ds:si], 0xFF

    pop ax
    mov [es:di], al
    pop ax
    mov [ds:si], al

    mov ax, 0
    je .check_done
    mov ax, 1

.check_done:
    pop si
    pop di
    pop es
    pop ds
    popf
    ret

kbd_wait:
    in al, 0x64
    test al, 0x02
    jnz kbd_wait
    ret


; detect_memory, ask the BIOS for a physical memory map using E820
; E820 returns one entry per call. you pass a continuation value in EBX (0 for first call),
; and the BIOS gives you back one entry + a new continuation value. when EBX comes back 0
; that was the last entry. each entry is 24 bytes: 8 base + 8 length + 4 type + 4 ACPI attrs
; the magic number 0x534D4150 is just "SMAP" in ASCII BIOS checks it on input and echoes it back
; if E820 isn't supported we fall back to E801 which is dumber (just gives total RAM, no map)
; but at least lets us boot
; nukes: basically everything

detect_memory:
    mov si, msg_mmap_e820
    call print_string

    mov di, MMAP_DATA           ; ES:DI points to where the first entry goes
    xor ebx, ebx               ; continuation value = 0 (start of list)
    mov dword [MMAP_ENTRIES], 0 ; no entries yet

.e820_loop:
    ; zero out the entry buffer before each call. if the BIOS only writes 20 bytes
    ; (old style, no ACPI extended attrs) the last 4 bytes stay 0 instead of garbage
    mov dword [es:di + 20], 0

    mov eax, 0x0000E820         ; E820 function number
    mov ecx, MMAP_ENTRY_SIZE    ; we can handle 24-byte entries
    mov edx, 0x534D4150         ; "SMAP" magic, BIOS won't do anything without this
    int 0x15

    jc .e820_fail               ; CF=1 means error or not supported

    cmp eax, 0x534D4150         ; BIOS should echo "SMAP" back to prove it understood
    jne .e820_fail

    ; make sure the BIOS actually gave us a real entry (at least 20 bytes)
    cmp cl, 20
    jb .e820_skip               ; runt entry, skip it

    ; force the ACPI 3.0 "this entry is valid" bit. if the BIOS wrote 24 bytes it
    ; might already be set, if it only wrote 20 we zeroed the field above so we
    ; need to set it ourselves. this way the kernel can treat all entries the same
    or dword [es:di + 20], 1

    ; some BIOSes return entries with length 0 for whatever reason. useless, skip em
    mov eax, [es:di + 8]       ; low 32 bits of length
    or eax, [es:di + 12]       ; OR with high 32 bits, if both are 0 the whole thing is 0
    jz .e820_skip

    ; good entry, advance the buffer pointer and bump the count
    inc dword [MMAP_ENTRIES]
    add di, MMAP_ENTRY_SIZE

.e820_skip:
    ; EBX = 0 means that was the last entry
    test ebx, ebx
    jz .e820_done

    jmp .e820_loop

.e820_fail:
    ; did we at least get some entries before it crapped out?
    cmp dword [MMAP_ENTRIES], 0
    jg .e820_done               ; partial success, use what we got

    ; total failure, try the older dumber method
    jmp .try_e801

.e820_done:
    mov si, msg_mmap_count
    call print_string
    mov ax, [MMAP_ENTRIES]
    call print_hex
    mov si, msg_crlf
    call print_string
    ret

.try_e801:
    ; E820 didn't work, fall back to INT 0x15 AX=0xE801
    ; this just gives us two numbers: memory between 1-16MB (in 1KB blocks) and
    ; memory above 16MB (in 64KB blocks). no detailed map, no reserved regions,
    ; but enough to know how much RAM we have. we fake two E820 entries from it
    mov si, msg_mmap_e801
    call print_string

    mov ax, 0xE801
    int 0x15
    jc .mmap_fail

    ; some BIOSes put the values in CX/DX instead of AX/BX. if CX is nonzero use that
    test cx, cx
    jz .use_ax
    mov ax, cx
    mov bx, dx
.use_ax:
    ; AX = 1KB blocks between 1MB and 16MB
    ; BX = 64KB blocks above 16MB
    ; we'll fake two entries: one for conventional memory, one for extended

    mov di, MMAP_DATA

    ; entry 0: conventional memory 0x0 - 0x9FC00 (639KB, the standard amount)
    ; every PC has this, it's the low memory below the VGA hole
    mov dword [es:di + 0], 0x00000000   ; base low
    mov dword [es:di + 4], 0x00000000   ; base high
    mov dword [es:di + 8], 0x0009FC00   ; length low (639KB)
    mov dword [es:di + 12], 0x00000000  ; length high
    mov dword [es:di + 16], 1           ; type = usable
    mov dword [es:di + 20], 1           ; ACPI valid bit
    add di, MMAP_ENTRY_SIZE

    ; entry 1: extended memory starting at 1MB
    ; total bytes = AX * 1024 + BX * 65536
    mov dword [es:di + 0], 0x00100000   ; base = 1MB mark
    mov dword [es:di + 4], 0x00000000   ; base high

    movzx eax, ax
    shl eax, 10                         ; AX * 1024
    movzx ebx, bx
    shl ebx, 16                         ; BX * 65536
    add eax, ebx                        ; total extended memory in bytes

    mov [es:di + 8], eax                ; length low
    mov dword [es:di + 12], 0x00000000  ; length high
    mov dword [es:di + 16], 1           ; type = usable
    mov dword [es:di + 20], 1           ; ACPI valid bit

    mov dword [MMAP_ENTRIES], 2

    mov si, msg_mmap_count
    call print_string
    mov ax, 2
    call print_hex
    mov si, msg_crlf
    call print_string
    ret

.mmap_fail:
    ; both E820 and E801 failed. we're flying blind on memory. not ideal
    mov si, msg_mmap_fail
    call print_string
    ret


; print_memory_map, dump all E820 entries to screen
; prints base address, length, and type for each entry so we can eyeball it
; format: "  base=0xHHHHLLLL len=0xHHHHLLLL type=N"
; we only print the low 32 bits of base/length since we're a 32-bit OS anyway
; nukes: everything (but who cares, this is just debug output)

print_memory_map:
    mov cx, [MMAP_ENTRIES]
    test cx, cx
    jz .pm_done

    mov di, MMAP_DATA

.pm_loop:
    push cx                     ; save loop counter, print_string/print_hex nuke CX
    push di                     ; save entry pointer too just to be safe

    ; "  base=0x"
    mov si, msg_mm_base
    call print_string

    ; print base address as 8 hex digits (high word first, then low word)
    ; print_hex_raw only does 16 bits at a time so we call it twice, the "0x" is already
    ; in the msg_mm_base string so we use _raw to avoid double-prefixing
    pop di                      ; restore DI so we can read from the entry
    push di                     ; and save it again for the rest of the loop
    mov ax, [es:di + 2]        ; high word of base_low dword
    call print_hex_raw
    pop di
    push di
    mov ax, [es:di + 0]        ; low word of base_low dword
    call print_hex_raw

    ; " len=0x"
    mov si, msg_mm_len
    call print_string

    pop di
    push di
    mov ax, [es:di + 10]       ; high word of length_low dword
    call print_hex_raw
    pop di
    push di
    mov ax, [es:di + 8]        ; low word of length_low dword
    call print_hex_raw

    ; " type="
    mov si, msg_mm_type
    call print_string

    ; type is a single digit 1-5, just print it as a char
    pop di
    push di
    mov al, [es:di + 16]
    add al, '0'
    mov ah, 0x0E
    xor bh, bh
    int 0x10

    mov si, msg_crlf
    call print_string

    pop di
    add di, MMAP_ENTRY_SIZE    ; advance to next entry
    pop cx
    dec cx
    jnz .pm_loop

.pm_done:
    ret


; read_sectors, read sectors from disk using LBA extended read (INT 0x13 AH=0x42)
; Input:
;  EAX = starting LBA sector number
;  CX = number of sectors to read
;  ES:BX = destination buffer
; Output:
;  CF=0 on success, CF=1 on error
; nukes: nothing (pusha preserves all GPRs, flags survive popa)

read_sectors:
    pusha

    ; fill in the disk address packet
    mov [dap_lba], eax ; which sector on disk
    mov [dap_count], cx ; how many sectors
    mov [dap_offset], bx ; buffer offset
    mov [dap_segment], es ; buffer segment

    ; INT 0x13 AH=0x42: extended read sectors
    ; DS:SI = pointer to DAP, DL = drive number
    mov ah, 0x42
    mov dl, [boot_drive]
    mov si, dap
    int 0x13
    ; CF is set by BIOS on failure, cleared on success

    popa ; restore all GPRs; popa does NOT touch flags
    ret ; CF from INT 0x13 survives to caller

; fat16_init: read the BPB from sector 0, parse filesystem geometry, cache FAT table
; Input: none (uses boot_drive)
; Output: all fat16_* variables populated, FAT table loaded at FAT_BUF
; nukes: everything (called once during init, don't care)

fat16_init:
    mov si, msg_fat16_init
    call print_string

    ; step 1: read sector 0 (boot sector containing BPB) into SECTOR_BUF
    push es
    xor ax, ax
    mov es, ax ; ES = 0
    mov bx, SECTOR_BUF ; ES:BX = 0x0000:0x0500
    xor eax, eax ; LBA 0
    mov cx, 1 ; 1 sector
    call read_sectors
    jc .init_fail

    ; step 2: parse BPB fields at known offsets
    ; the BPB sits at fixed byte offsets inside the boot sector, defined by the FAT spec. we just read them out and stash them in our variables
    mov si, SECTOR_BUF

    mov ax, [si + 0x0B] ; offset 0x0B: bytes per sector
    mov [fat16_bps], ax

    mov al, [si + 0x0D] ; offset 0x0D: sectors per cluster
    mov [fat16_spc], al

    mov ax, [si + 0x0E] ; offset 0x0E: reserved sectors
    mov [fat16_reserved], ax

    mov al, [si + 0x10] ; offset 0x10: number of FATs
    mov [fat16_num_fats], al

    mov ax, [si + 0x11] ; offset 0x11: max root dir entries
    mov [fat16_root_entries], ax

    mov ax, [si + 0x16] ; offset 0x16: sectors per FAT
    mov [fat16_fat_size], ax

    ; step 3: calculate derived sector positions
    ; fat_start = reserved_sectors
    ; the FAT table begins right after the reserved area
    mov ax, [fat16_reserved]
    mov [fat16_fat_start], ax

    ; root_dir_start = reserved + (num_fats * fat_size)
    ; root dir begins after all FAT copies
    xor ah, ah
    mov al, [fat16_num_fats]
    mul word [fat16_fat_size] ; AX = num_fats * fat_size (DX nuked)
    add ax, [fat16_reserved]
    mov [fat16_root_start], ax

    ; root_dir_sectors = root_entry_count * 32 / 512 = root_entry_count / 16
    ; each dir entry is 32 bytes, each sector is 512 bytes
    mov ax, [fat16_root_entries]
    shr ax, 4 ; divide by 16
    mov [fat16_root_sectors], ax

    ; data_state = root_dir_start + root_dir_sectors
    ; data clusters begin right after the root dir
    add ax, [fat16_root_start]
    mov [fat16_data_start], ax

    ; step 4: load the entire FAT table into FAT_BUF
    ; we cache it in RAM so cluster chain lookups are just memory reads
    movzx eax, word [fat16_fat_start]
    mov cx, [fat16_fat_size] ; how many sectors the FAT occupies
    mov bx, FAT_BUF ; ES:BX = 0x0000:0x1000
    call read_sectors
    jc .init_fail

    pop es

    mov si, msg_fat16_ok
    call print_string
    ret

.init_fail:
    pop es
    mov si, msg_fat16_fail
    call print_string
    ret


; fat16_find_file: scan root dir for TUPLEOS.BIN
; Input: none (uses fat16_kernel_name and parsed BPB values)
; Output:
;  AX = first cluster of file (0 if not found)
;  fat16_file_size populated with file size in bytes
; nukes: everything

fat16_find_file:
    mov si, msg_fat16_scan
    call print_string

    movzx eax, word [fat16_root_start] ; LBA of first root dir sector
    mov cx, [fat16_root_sectors] ; how many sectors in root dir

.scan_sector:
    push cx ; save remaining sector count
    push eax ; save current LBA

    ; read one root dir sector into SECTOR_BUF
    push es
    xor bx, bx
    mov es, bx
    mov bx, SECTOR_BUF ; ES:BX = 0x0000:0x0500
    mov cx, 1
    call read_sectors
    pop es

    pop eax ; restore LBA
    jc .find_not_found

    ; each sector holds 512/32 = 16 dir entries
    mov di, SECTOR_BUF
    mov dx, 16

.check_entry:
    ; first byte 0x00 = no more entries in dir, we're done
    cmp byte [di], 0x00
    je .find_not_found

    ; first byte 0xE5 = entry is free (was deleted), skip it
    cmp byte [di], 0xE5
    je .skip_entry

    ; check attribute byte at offset 0x0B for LFN market (0x0F)
    ; long filename entries look nothing like real entries, skip them
    cmp byte [di + 0x0B], 0x0F
    je .skip_entry

    ; compare 11 bytes: 8 char name + 3 char extension
    ; pusha/popa save all GPRs; repe cmpsb sets ZF if all 11 matched
    ; popa does NOT touch flags, so ZF survives for the je below
    pusha
    mov si, fat16_kernel_name
    mov cx, 11
    repe cmpsb
    popa
    je .find_found

.skip_entry:
    add di, 32 ; advance to next 32-byte dir entry
    dec dx
    jnz .check_entry

    ; exhausted this sector, move to the next one
    pop cx ; restore remaining sector count
    inc eax ; next sector LBA
    dec cx
    jnz .scan_sector

    ; fell through all sectors without a match
    xor ax, ax
    ret

.find_not_found:
    pop cx ; balance the push from .scan_sector
    mov si, msg_fat16_nf
    call print_string
    xor ax, ax ; return 0 = not found
    ret

.find_found:
    ; DI still points to the matching dir entry
    ; offset 0x1A = first cluster number (16-bit)
    ; offset 0x1C = file size (32-bit)
    mov ecx, [di + 0x1C] ; file size in bytes
    mov [fat16_file_size], ecx
    mov ax, [di + 0x1A] ; first cluster number

    pop cx ; balance the push from .scan_sector

    mov si, msg_fat16_found
    call print_string
    ret

; fat16_load_file: follow the FAT cluster chain and load the entire file into memory
; Input:
;  AX = first cluster number (from fat16_find_file)
; Output:
;  File contents at physical 0x10000 (KERNEL_SEG:KERNEL_OFF)
; Nukes: everything

; how it works:
; 1. convert cluster number to LBA: lba = data_start + (cluster - 2) * sectors_per_cluster
; 2. read spc sectors into current load segment
; 3. advance the load segment by spc * 32 paragraphs (= spc * 512 / 16)
; 4. look up next cluster in the cached FAT table: next = [FAT_BUF + cluster * 2]
; 5. if next < 0xFFF8, loop. Otherwise, end of file.

fat16_load_file:
    mov [fat16_curr_cluster], ax
    mov word [fat16_load_seg], KERNEL_SEG

.load_cluster:
    ; convert cluster to LBA
    ; lba = data_start + (cluster - 2) * sectors_per_cluster
    ; clusters 0 and 1 are reserved in FAT, so cluster 2 = first data sector
    mov ax, [fat16_curr_cluster]
    sub ax, 2
    xor dx, dx
    mov dl, [fat16_spc]
    mul dx ; AX = (cluster - 2) * spc
    add ax, [fat16_data_start]
    movzx eax, ax ; zero-extend for read_sectors

    ; read one cluster into [fat16_load_seg]:0x0000
    push es
    mov es, [fat16_load_seg]
    xor bx, bx ; offset 0 within segment
    xor ch, ch
    mov cl, [fat16_spc] ; CX = sectors per cluster
    call read_sectors
    pop es
    jc .load_fail

    ; advance load pointer
    ; each cluster is spc * 512 bytes. in segment terms that's spc * 32 paragraphs
    ; (1 paragraph = 16 bytes, so 512 / 16 = 32)
    xor ax, ax
    mov al, [fat16_spc]
    shl ax, 5 ; * 32
    add [fat16_load_seg], ax

    ; follow the FAT chain
    ; FAT16 entries are 2 bytes each. entry N is at FAT_BUF + N*2
    ; the value tells us the next cluster, or >=0xFFF8 means end of chain
    mov si, [fat16_curr_cluster]
    shl si, 1 ; * 2 (byte offset into FAT)
    add si, FAT_BUF
    mov ax, [ds:si] ; AX = next cluster from FAT
    mov [fat16_curr_cluster], ax

    cmp ax, 0xFFF8
    jb .load_cluster ; below 0xFFF8 = valid next cluster

    ; Done, entire file is in memory
    mov si, msg_fat16_load
    call print_string
    ret

.load_fail:
    mov si, msg_fat16_rerr
    call print_string
    ret


; 32-bit protected mode code

; everything here will run in 32-bit protected mode. the [BITS 32] directive will tell NASM to assemble 32-bit opcodes, it uses the same mnemonics (like mov, add, jmp) but the machine code bytes are different
; in 16-but mode, a "mov eax" call has a 0x66 prefix byte, in 32-bit mode it doesn'table
; naturally if BITS is wrong, the CPU will decode garbage and crash, gg

; BIOS interrupts are completely dead. the only way to put text on screen is to write directly to the VGA text buffer at phyiscal addr 0xB8000.
; the VGA controller watches that memory range and renders whatever bytes are there onto the display

; VGA text buffer layout:
;  80 cols x 25 rows = 2000 char cells
;  each cell is 2 bytes: [byte 0: ASCII char] [byte 1: attribute]
;  attribute format: bits 0-3 = foreground, bits 4-7 = background color
;  row 0 starts at 0xB8000, row 1 at 0xB80A0, row N at B8000 + N*160
;  0x0A = background 0x0 (black) + foreground + 0xA (bright green)

[BITS 32]

pm_entry:
    ; load all data segment registers with the kernal data selector (0x10)
    ; in real mode these held base addresses (shifted left 4 to form 20-bit physical addrs)
    ; in protected mode they hold selectors which are byte offsets into the GDT
    ; the cpu looks up the GDT entry at that offset and uses it to determine the segment's base addr, size limit, and perms

    ; 0x10 = byte offset 16 = third GDT entry = gdt_data, which is our flat data segment: base 0, limit 4GB, read/write, ring 0.
    ; with base 0, the effective address is just the offset: mov [0xB8000] really writes to physical addr 0xB8000, no translation or anything yay

    ; CS was already set to 0x08 by the far jump, we need to set the rest manually because the CPU is autistic and doesn't touch them during the mode switch
    ; they will hold on to whatever real-mode values they had, which are now garbage and meaningless
    ; (or even worse, they are valid but with the wrong selectors if they just so happen to exist in our GDT)


    ; we are going through AX because x86 be hella federal and doesn't allow mov ds, 0x10 (immediate to segment register)
    ; same rule as real mode, still applies in protected mode
    mov ax, 0x10 ; data segment selector
    mov ds, ax ; data segment, used by most MOV/string operations
    mov es, ax ; extra segment, used by string ops
    mov fs, ax ; general puirpose segment, not sure what this is used for unless explicitely defined but we will zero it anyway
    mov gs, ax ; so nothing points to stale real-mode junk
    mov ss, ax ; stack segment, this MUST ABSOLUTELY match our data segment

    ; set up a 32-bit stack pointer. the real-mode stack was at 0x7C00 with 16-bit SP, only good for ~30KB below the MBR. now we have 32-bit ESP and the full address space (560+KB)
    ; stage 2 (which ends at 0xBE00) gives us a cap which is plenty for free conventional memory
    ; the stack grows downard from the address above, giving us hundreds of KB of room, way more than we will ever need in the bootloader
    mov esp, 0x90000

    ; proof of life message, we will write directly to VGA memory to prove we're in 32-bit protected mode.
    ; if we can write to 0xB8000 and see green text on screen, it means GDT is valid, the far jump worked, data segments work, and the stack is fine

    ; ESI = source string address. we use ESI and not SI cause we're in 32 bit now naturally
    mov esi, msg_pm_ok
    ; OIKJWEFJIOWFEIWEFJIOWEFIJOWEFIJOEWIFJOJIWEOFIJOEWFIJOEWFIOJWEFIJO
    mov edi, 0xB8000 + 3680

    mov ah, 0x0A

.pm_print:
    ; LODSB: load byte at [DS:ESI] into AL, then increment ESI
    ; same instruction as 16-bit print_string but now it uses ESI
    lodsb

    ; check for null term. our strings null terminated just like real mode
    ; we can test al, al does a bitwise AND without storing the reult, so it just sets the zero flag if AL is set 0
    test al, al
    jz .pm_halt ; end of string

    ;write char to VGA
    mov [edi], ax

    ; advance the EDI by 2 to point to the next VGA cell cause each cell is 2 bytes
    add edi, 2

    ; loop back for the next char
    jmp .pm_print

.pm_halt:
    ; we done now, later we will jump to the loaded kernel from here, for now just finna halt. 
    ; cli is technically redundant (we already ran before switch) but it's good practice probably to halt in a loop cause it will make the intent clearer
    ; and probably can guard against someone adding something else later
    cli
    hlt
    ; here we catch non-maskable interrupts, if the JMP sends us back to the HLT over and over the cpu will just spin here forever
    jmp .pm_halt





; data


; GDT (Global descriptor table)
; the GDT is an in memory table that describes memory segments.
; in protected mode, every time the CPU accesses memory it checks the active segment descriptor to enforce base addr, size limit, perms, and privilege level. the CPU refuses to execute code or access data without a valid GDT

; we use a flat model: both our segments (code + data) have base=0 and limit=4GB
; this means segment:offset = 0 + offset = just the offset
; segmentation does nothing, this is how every modern OS does it
; set up a flat GDT to satisfy the CPU's requirement, then use paging for real memory protection
; our kernel's paging code will handle that

; each GDT entry is 8 bytes. the field layout is ugly because intel designed the original GDT for 6 byte entries.
; then had to bolt on extra bits for 32 bit mode without breaking backwards compatibility (lol mongs)
; so the base and limit fields are split across non-contiguous byte positions:

; byte 0-1: Limit[0:15] low 16 bits of the segment size
; byte 2-3: Base[0:15] low 16 bits of the segment start addr
; byte 4: Base[16:23] next 8 bits of the base
; byte 5: access byte which has flags: present, privilege, type, permissions
; byte 6: high nibble = Flags (granularity, 32-bit, etc.), low nibble = Limit[16:19] (top 4 bits of the limit)
; byte 7: Base[24:31] top 8 bits of base

; for our entries: base 0x00000000 (all base bytes = 0x00), limit = 0xFFFFF (all limit bits = 1), with granularity=1 (4KB pages), effective limit = 0xFFFFF * 4KB = 4GB

; Access byte bit layout:
; bit 0: A (accessed) cpu sets this to 1 on first use, we start at 0
; bit 1: RW (read/write) if E=0, RW 0 = read-only, RW 1 = read + write, if E=1, RW 0 =execute only, (can't read the data but can excute), RW 1 = execute + read
; bit 2: DC (dir/conform) 0 for both: data greows up, code is nonconforming
; bit 3: E (executable) 1 = code segment, 0 = data segment
; bit 4: S (type) 1 = code/data segment, 0 = system (TSS, gate, etc.)
; bit 5-6 DPL (privilege) 00 = ring 0 (kernel only), 11 = ring 3 (user)
; bit 7: P (present) 1 = segment exists in memory, 0 = CPU faults on use

; Flags nibble (top 4 bits of byte 6)
; bit 4: reserved, always 0
; bit 5: L (long mode) 1= 64-bit, 0 = not, we use 0 (until we add 64 bit architecture)
; bit 6: DB (size) 1 = 32-bit segment, 0 = 16-bit
; bit 7: G (granularity) 1 = limit is in 4KB pages, 0 = limit is in bytes

; we need to align to 8 bytes. probably not strictly required by CPU but the GDT is accessed frequently and aligning it will avoid split cache line reads. costs nothing and probably will save me hours of headache
align 8

gdt_start:
; gdt_descriptor will reference this address so the LGDT knows where the table starts

gdt_null:
    ; entry 0: the null descriptor, the CPU requires the first GDT entry to be all zeros, selector 0x00 is treated as "no segment" if you accidentally load 0x00 into DS then try to read memory, the CPU will fault
    dq 0

gdt_code:
    ; entry 1: kernel code segment. selector = 0x08 (8 bytes into the GDT)
    ; this is what CS gets loaded during the far jump
    dw 0xFFFF ; limit[0:15] all 1's
    dw 0x0000 ; base[0:15] all 0
    db 0x00 ; base[16:23] all 0

    ; access byte, present 1, kernel privilege, not system data, executable, non conforming, readable, not yet accessed
    db 10011010b

    ; flags = limit is 4KB pages, 32-bit, not long mode, reserved bit
    db 11001111b

    ; base [24:31] = 0x00
    db 0x00

gdt_data:
    ; entry 2 kernel data segment selector = 0x10 (16 bytes into GDT)
    ; this is what ds, es, fs, gs ,ss get loaded with after switch
    ; identical to gdt_code except E=0(not executable) and RW=1 means "writable"

    dw 0xFFFF ; limit[0:15] all 1's
    dw 0x000
    db 0x00

    ; present, kernel level, not executable, writable, not accessed yet, segment exists, segment grows up
    db 10010010b

    ; flags
    db 11001111b

    db 0x00

gdt_end:
    ; label marking the end of the GDT

gdt_descriptor:
    dw gdt_end - gdt_start - 1
    dd gdt_start


banner_top: db "+========================+", 13, 10, 0
banner_title: db"|      MyBoot v1.0       |", 13, 10, 0
banner_bottom: db"+========================+", 13, 10, 0

msg_drive: db "Boot drive: ", 0
msg_crlf: db 13, 10, 0
msg_ok: db "Stage 2 loaded successfully.", 13, 10, 0
msg_a20_bios:   db "A20: trying BIOS...", 13, 10, 0
msg_a20_kbd:    db "A20: trying keyboard controller...", 13, 10, 0
msg_a20_fast:   db "A20: trying fast A20 (port 0x92)...", 13, 10, 0
msg_a20_ok:     db "A20: enabled.", 13, 10, 0
msg_a20_fail:   db "A20: FAILED - cannot continue.", 13, 10, 0
msg_mmap_e820:  db "Memory: detecting via E820...", 13, 10, 0
msg_mmap_e801:  db "Memory: E820 failed, trying E801...", 13, 10, 0
msg_mmap_count: db "Memory: found ", 0
msg_mmap_hdr:   db "Memory map:", 13, 10, 0
msg_mmap_fail:  db "Memory: FAILED - no memory map!", 13, 10, 0
msg_mm_base:    db "  base=0x", 0
msg_mm_len:     db " len=0x", 0
msg_mm_type:    db " type=", 0

boot_drive: db 0

msg_pm: db "Loading GDT, switching to protected mode...", 13, 10, 0
msg_pm_ok: db "[ OK ] 32-bit protected mode active!", 0

; FAT16 parsed BPB values (populated by fat16_init)
fat16_bps: dw 0 ; bytes per sector (always 512 )
fat16_spc: db 0 ; sectors per cluster, power of 2: (1,2,4,8,16,32,64,128)
fat16_reserved: dw 0 ; reserved sectors count, from the start of the partition to the start of the first FAT table, usually 1 for MBR partitions
fat16_num_fats: db 0 ; number of FAT tables, usually 2
fat16_root_entries: dw 0 ; max root dir entries
fat16_fat_size: dw 0 ; sectors per FAT table

; Derived values (calculated from BPB by fat16_init)
fat16_fat_start: dw 0 ; LBA of the first FAT table, = reserved sectors count
fat16_root_start: dw 0 ; LBA of the root directory, = reserved sectors
fat16_root_sectors: dw 0 ; sectors occupied by the root directory, = (max root entries * 32) / bytes per sector
fat16_data_start: dw 0 ; LBA of the first data cluster, = reserved sectors + (num FATs * sectors per FAT) + root dir sectors

; Runtime state
fat16_curr_cluster: dw 0 ; current cluster being read
fat16_load_seg: dw 0 ; current segment for kernel loading
fat16_file_size: dd 0 ; file size from dir entry

; Filename to search for (8.3 format: 8 chars name + 3 chars extension, space-padded, no dot)
fat16_kernel_name: db "TUPLEOS BIN"

; Dis address packet for INT 0x13 AH=0x42 (LBA extended read)
; the cpu reads this struct from memory when we call INT 0x13/AH=0x42 to know where to put the data we read from disk
align 4
dap: db 0x10 ; packet size (always 16)
     db 0 ; reserved
dap_count: dw 0 ; sectors to read
dap_offset: dw 0 ; destination offset
dap_segment: dw 0; destination segment
dap_lba: dd 0 ; LBA low 32 bits
         dd 0 ; LBA high 32 bits

; FAT16 status messages
msg_fat16_init: db "FAT16: reading BPB...", 13, 10, 0
msg_fat16_ok: db "FAT16: filesystem initialized.", 13, 10, 0
msg_fat16_fail: db "FAT16: failed to read BPB", 13, 10, 0
msg_fat16_scan: db "FAT16: searching for TUPLEOS.BIN...", 13, 10, 0
msg_fat16_found: db "FAT16: file found, loading...", 13, 10, 0
msg_fat16_load: db "FAT16: kernel loaded to 0x10000", 13, 10, 0
msg_fat16_nf: db "FAT16: TUPLEOS.BIN not found!", 13, 10, 0
msg_fat16_rerr: db "FAT16: disk read error!", 13, 10, 0
