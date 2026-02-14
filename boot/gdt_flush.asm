# After we build the GDT table in C, we need to:
# 1. Run `lgdt` to tell the CPU where our GDT lives
# 2. Do a "far jump" to reload the code segment register
# 3. Reload all other segment registers (DS, ES, FS, GS, SS)

# We can't do this in C because `lgdt` is a privilged instruction with no C equivalent
# reloading CS requires a far jump (jmp segment:offset), which C compilers can't generate
# and the other segment registers need manual reloading too

.section .text
.global gdt_flush
.type gdt_flush, @function


gdt_flush:
    # Grab the argument: a pointer to our gdt_ptr struct
    # In the C calling convention, the first argument is at [esp+4] (esp+0 is the return address)
    
    mov 4(%esp), %eax

    # lgdt = "Load Global Descriptor Table Register"
    # Tells the CPU: "here's where the GDT lives and how big it is"
    # The cpu reads the 6-byte struct (2 byte limit + 4 byte base)
    lgdt (%eax)

    # Now we need to reload all segment registers to use our NEW GDT
    # Segment registers hold "selectors, indices into the GDT

    # 0x10 = 16 in decimal = 0000 0000 0001 0000 in binary
    #   bits 3-15 = 00000000010 = index 2 (our kernel data segment)
    #   bit  2    = 0 = GDT (not LDT)
    #   bits 0-1  = 00 = ring 0 (kernel privilege)

    # So 0x10 means "GDT entry #2, ring 0" = our kernel data segment
    mov $0x10, %ax
    mov %ax, %ds    # Data segment
    mov %ax, %es    # Extra segment
    mov %ax, %fs    # General-purpose segment
    mov %ax, %gs    # General-purpose segment
    mov %ax, %ss    # Stack segment

    # Reload code segment with a far jump
    # CS can ONLY be changed bny a far jump or far return, not by mov

    # 0x08 = 8 in decimal = 0000 0000 0000 1000 in binary
    #   bits 3-15 = 00000000001 = index 1 (our kernel code segment)
    #   bit  2    = 0 = GDT (not LDT)
    #   bits 0-1  = 00 = ring 0 (kernel privilege)

    # This jumps to the very next instruction, but the ACT of jumping forces the CPU to reload CS with our new code segment
jmp $0x08, $flush_done

flush_done:
    ret

.size gdt_flush, . - gdt_flush



.global tss_flush
.type tss_flush, @function

tss_flush:
    mov 4(%esp), %ax # Get the TSS selecotr argument (0x28) I think??????
    ltr %ax # Load task register
    ret

.size tss_flush, . - tss_flush
