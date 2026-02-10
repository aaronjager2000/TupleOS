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


; data
banner_top: db "+========================+", 13, 10, 0
banner_title: db"|      MyBoot v1.0       |", 13, 10, 0
banner_bottom: db"+========================+", 13, 10, 0

msg_drive: db "Boot drive: ", 0
msg_crlf: db 13, 10, 0
msg_ok: db "Stage 2 loaded successfully.", 13, 10, 0

boot_drive: db 0
