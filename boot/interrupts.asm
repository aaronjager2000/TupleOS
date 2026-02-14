# interrupts.asm - Assembly stubs for all interrupts
#
# Why do we need assembly stubs at all?
# When an interrupt fires, the CPU pushes some state (EIP, CS, EFLAGS)
# but NOT the general-purpose registers. If our C handler used EAX
# to do math, and we returned without restoring it, the interrupted
# code would find EAX mysteriously changed. Chaos.
#
# So each stub does:
#   1. Push a dummy error code (if the CPU didn't push one)
#   2. Push the interrupt number
#   3. Jump to common code that saves ALL registers
#   4. Call our C handler (interrupt_handler in idt.c)
#   5. Restore ALL registers
#   6. iret (return from interrupt)

.section .text
.extern interrupt_handler

# CPU EXCEPTIONS (interrupts 0-31)
# Some exceptions push an error code automatically, some don't.
# For the ones that don't, we push a dummy 0 so the stack
# layout is identical for ALL interrupts. This lets us use
# one common handler without special cases.
#
# Exceptions that push an error code: 8, 10, 11, 12, 13, 14, 17, 21, 29, 30
# All others: we push 0 ourselves.

# Exceptions WITHOUT an error code (we push 0) 

.global isr0
isr0:   # Division by Zero
    push $0     # dummy error code
    push $0     # interrupt number
    jmp isr_common

.global isr1
isr1:   # Debug
    push $0
    push $1
    jmp isr_common

.global isr2
isr2:   # Non-Maskable Interrupt
    push $0
    push $2
    jmp isr_common

.global isr3
isr3:   # Breakpoint
    push $0
    push $3
    jmp isr_common

.global isr4
isr4:   # Overflow
    push $0
    push $4
    jmp isr_common

.global isr5
isr5:   # Bound Range Exceeded
    push $0
    push $5
    jmp isr_common

.global isr6
isr6:   # Invalid Opcode
    push $0
    push $6
    jmp isr_common

.global isr7
isr7:   # Device Not Available
    push $0
    push $7
    jmp isr_common

# Exception WITH error code (CPU pushes it, we don't) 

.global isr8
isr8:   # Double Fault
    # CPU already pushed error code!
    push $8
    jmp isr_common

# Back to WITHOUT error code 

.global isr9
isr9:   # Coprocessor Segment Overrun (legacy)
    push $0
    push $9
    jmp isr_common

# WITH error code 

.global isr10
isr10:  # Invalid TSS
    push $10
    jmp isr_common

.global isr11
isr11:  # Segment Not Present
    push $11
    jmp isr_common

.global isr12
isr12:  # Stack-Segment Fault
    push $12
    jmp isr_common

.global isr13
isr13:  # General Protection Fault
    push $13
    jmp isr_common

.global isr14
isr14:  # Page Fault
    push $14
    jmp isr_common

# --- WITHOUT error code ---

.global isr15
isr15:  # Reserved
    push $0
    push $15
    jmp isr_common

.global isr16
isr16:  # x87 Floating-Point Exception
    push $0
    push $16
    jmp isr_common

# --- WITH error code ---

.global isr17
isr17:  # Alignment Check
    push $17
    jmp isr_common

# --- WITHOUT error code ---

.global isr18
isr18:  # Machine Check
    push $0
    push $18
    jmp isr_common

.global isr19
isr19:  # SIMD Floating-Point Exception
    push $0
    push $19
    jmp isr_common

.global isr20
isr20:  # Virtualization Exception
    push $0
    push $20
    jmp isr_common

# WITH error code 

.global isr21
isr21:  # Control Protection Exception
    push $21
    jmp isr_common

# WITHOUT error code (22-28 are reserved) 

.global isr22
isr22:
    push $0
    push $22
    jmp isr_common

.global isr23
isr23:
    push $0
    push $23
    jmp isr_common

.global isr24
isr24:
    push $0
    push $24
    jmp isr_common

.global isr25
isr25:
    push $0
    push $25
    jmp isr_common

.global isr26
isr26:
    push $0
    push $26
    jmp isr_common

.global isr27
isr27:
    push $0
    push $27
    jmp isr_common

.global isr28
isr28:
    push $0
    push $28
    jmp isr_common

# --- WITH error code ---

.global isr29
isr29:  # VMM Communication Exception
    push $29
    jmp isr_common

.global isr30
isr30:  # Security Exception
    push $30
    jmp isr_common

# --- WITHOUT error code ---

.global isr31
isr31:  # Reserved
    push $0
    push $31
    jmp isr_common



# HARDWARE IRQs (mapped to interrupts 32-47 after PIC remap)

# Hardware IRQs never have CPU error codes, so we always push 0.
# We push the REMAPPED interrupt number (32+), not the raw IRQ number.

.global irq0
irq0:   # PIT Timer (fires ~18.2 times/sec by default)
    push $0
    push $32
    jmp isr_common

.global irq1
irq1:   # Keyboard (fires on every key press AND release)
    push $0
    push $33
    jmp isr_common

.global irq2
irq2:   # Cascade (used internally by PICs, never fires)
    push $0
    push $34
    jmp isr_common

.global irq3
irq3:   # COM2 serial port
    push $0
    push $35
    jmp isr_common

.global irq4
irq4:   # COM1 serial port
    push $0
    push $36
    jmp isr_common

.global irq5
irq5:   # LPT2 parallel port
    push $0
    push $37
    jmp isr_common

.global irq6
irq6:   # Floppy disk
    push $0
    push $38
    jmp isr_common

.global irq7
irq7:   # LPT1 parallel port (spurious)
    push $0
    push $39
    jmp isr_common

.global irq8
irq8:   # CMOS real-time clock
    push $0
    push $40
    jmp isr_common

.global irq9
irq9:   # Free / ACPI
    push $0
    push $41
    jmp isr_common

.global irq10
irq10:  # Free
    push $0
    push $42
    jmp isr_common

.global irq11
irq11:  # Free
    push $0
    push $43
    jmp isr_common

.global irq12
irq12:  # PS/2 Mouse
    push $0
    push $44
    jmp isr_common

.global irq13
irq13:  # FPU / Coprocessor
    push $0
    push $45
    jmp isr_common

.global irq14
irq14:  # Primary ATA hard disk
    push $0
    push $46
    jmp isr_common

.global irq15
irq15:  # Secondary ATA hard disk
    push $0
    push $47
    jmp isr_common


# ============================================================
# SYSCALL INTERRUPT (INT 0x80 = interrupt 128)
# ============================================================
# the gateway between user land and the kernel
# user code does INT 0x80 and ends up here
# software interrupt so no CPU error code, we push dummy 0

.global isr128
isr128:  # System Call
    push $0     # dummy error code
    push $128   # interrupt number
    jmp isr_common


# ============================================================
# COMMON HANDLER
# ============================================================
# Every stub above jumps here. At this point the stack looks like:
#
#   (top of stack)
#   interrupt_number   <- we pushed this
#   error_code         <- we pushed (or CPU pushed)
#   EIP                <- CPU pushed these 3 automatically
#   CS                     when the interrupt fired
#   EFLAGS
#   (bottom)
#
# We need to save the rest of the CPU state, call C, then restore it all.

isr_common:
    # Save all general-purpose registers.
    # pusha pushes: EAX, ECX, EDX, EBX, ESP, EBP, ESI, EDI (in that order)
    pusha

    # Save the data segment selector.
    # We're about to switch to kernel data segment, so save the current
    # one so we can restore it later (matters when we add user mode).
    mov %ds, %ax
    push %eax

    # Load kernel data segment (0x10 = GDT entry #2, our kernel data segment)
    # This ensures we can safely access kernel memory in our C handler.
    mov $0x10, %ax
    mov %ax, %ds
    mov %ax, %es
    mov %ax, %fs
    mov %ax, %gs

    # Call the C handler.
    # Push a pointer to the stack frame as the argument.
    # ESP currently points to all the stuff we pushed, which matches
    # the layout of struct interrupt_frame in idt.h exactly.
    push %esp
    call interrupt_handler
    add $4, %esp   # clean up the argument we pushed

    # Restore the original data segment
    pop %eax
    mov %ax, %ds
    mov %ax, %es
    mov %ax, %fs
    mov %ax, %gs

    # Restore all general-purpose registers
    popa

    # Clean up the error code and interrupt number we pushed
    # (add 8 to skip past those two 4-byte values on the stack)
    add $8, %esp

    # Return from interrupt.
    # iret pops EIP, CS, and EFLAGS that the CPU pushed,
    # restoring execution to wherever it was before the interrupt.
    iret
