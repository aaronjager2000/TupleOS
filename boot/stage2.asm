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

    ; future steps hook in here (GDT, protected mode, kernel load)

; Halt, nothing more to do in this step gg no re
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


; data
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
