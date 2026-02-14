#include "idt.h"
#include "ports.h"
#include <stddef.h>

// THIS TOOK 1.5 HOURS TO WRITE GG

/*
 * IDT setup, PIC remapping, and interrupt dispatch
 *
 * Three responsibilities:
 *   1. Build the 256-entry IDT table
 *   2. Remap the PIC so hardware IRQs don't collide with CPU exceptions
 *   3. Dispatch interrupts to registered C handler functions
 */

// The actual IDT: 256 entries, one per possible interrupt 
struct idt_entry idt[IDT_ENTRIES];

// Pointer struct that we feed to the `lidt` instruction 
struct idt_ptr idtp;

/* Array of C function pointers — one per interrupt.
 * When interrupt N fires, we call handlers[N] if it's not NULL.
 * Initially all NULL (no handlers registered).
 */
static interrupt_handler_t handlers[IDT_ENTRIES] = {0};

/* Assembly stubs defined in interrupts.asm.
 * Each one pushes the interrupt number onto the stack,
 * then jumps to a common handler that calls our C function.
 * We need one stub per interrupt because the CPU doesn't tell
 * us WHICH interrupt fired — the stub encodes that info.
 */
extern void isr0(void);
extern void isr1(void);
extern void isr2(void);
extern void isr3(void);
extern void isr4(void);
extern void isr5(void);
extern void isr6(void);
extern void isr7(void);
extern void isr8(void);
extern void isr9(void);
extern void isr10(void);
extern void isr11(void);
extern void isr12(void);
extern void isr13(void);
extern void isr14(void);
extern void isr15(void);
extern void isr16(void);
extern void isr17(void);
extern void isr18(void);
extern void isr19(void);
extern void isr20(void);
extern void isr21(void);
extern void isr22(void);
extern void isr23(void);
extern void isr24(void);
extern void isr25(void);
extern void isr26(void);
extern void isr27(void);
extern void isr28(void);
extern void isr29(void);
extern void isr30(void);
extern void isr31(void);

// Hardware IRQ stubs (IRQ 0-15 mapped to interrupts 32-47) 
extern void irq0(void);
extern void irq1(void);
extern void irq2(void);
extern void irq3(void);
extern void irq4(void);
extern void irq5(void);
extern void irq6(void);
extern void irq7(void);
extern void irq8(void);
extern void irq9(void);
extern void irq10(void);
extern void irq11(void);
extern void irq12(void);
extern void irq13(void);
extern void irq14(void);
extern void irq15(void);

// syscall interrupt (INT 0x80 = interrupt 128)
extern void isr128(void);

/*
 * Set one IDT entry.
 * Similar idea to gdt_set_entry — the handler address gets split in half.
 *
 * Parameters:
 *   index   - which interrupt (0-255)
 *   base    - address of the handler function
 *   selector - code segment selector (always 0x08 = our kernel code segment)
 *   flags   - type/attribute byte
 */
static void idt_set_entry(uint8_t index, uint32_t base, uint16_t selector, uint8_t flags) {
    idt[index].base_low  = base & 0xFFFF;         /* lower 16 bits of handler address */
    idt[index].base_high = (base >> 16) & 0xFFFF;  /* upper 16 bits of handler address */
    idt[index].selector  = selector;
    idt[index].zero      = 0;
    idt[index].flags     = flags;
}

/*
 * Remap the PIC (Programmable Interrupt Controller)
 *
 * The PC has two PICs chained together (master + slave) DONT CANCEL ME, controlling 16 hardware IRQ lines.
 *
 * Problem: By default, the master PIC maps IRQ 0-7 to interrupts 8-15.
 *          But interrupts 8-15 are ALSO used by CPU exceptions!
 *          (IRQ 0 = interrupt 8 = Double Fault?? That's chaos.)
 *
 * Solution: Reprogram the PICs to map IRQs to interrupts 32-47 instead,
 *           safely out of the way of the 32 CPU exceptions (0-31).
 *
 * PIC ports:
 *   Master PIC: command = 0x20, data = 0x21
 *   Slave PIC:  command = 0xA0, data = 0xA1
 *   DONT CANCEL ME I DIDNT COME UP WITH THESE NAMES LMAO
 *
 * The reprogramming sequence is 4 "Initialization Command Words" (ICW1-ICW4).
 * This is a rigid protocol defined by the Intel 8259 chip datasheet.
 */
static void pic_remap(void) {
    /* Save the current interrupt masks (which IRQs are enabled/disabled).
     * We'll restore them after remapping so we don't accidentally
     * enable/disable any IRQs during the process. */
    uint8_t mask_master = inb(0x21);
    uint8_t mask_slave  = inb(0xA1);

    /* ICW1: Start initialization sequence (0x11)
     * bit 0 = 1: ICW4 will be needed
     * bit 4 = 1: This is an initialization command
     * Sent to BOTH PICs */
    outb(0x20, 0x11);
    io_wait();
    outb(0xA0, 0x11);
    io_wait();

    /* ICW2: Set the base interrupt number
     * Master PIC: IRQ 0-7  -> interrupts 32-39 (0x20)
     * Slave PIC:  IRQ 8-15 -> interrupts 40-47 (0x28) */
    outb(0x21, 0x20);  // master offset: interrupt 32 
    io_wait();
    outb(0xA1, 0x28);  // slave offset: interrupt 40 
    io_wait();

    /* ICW3: Tell PICs how they're wired together
     * Master: bit 2 set = slave is connected on IRQ line 2 (0x04)
     * Slave:  value 2 = "I'm connected to master's IRQ 2" */
    outb(0x21, 0x04);
    io_wait();
    outb(0xA1, 0x02);
    io_wait();

    /* ICW4: Set operating mode (0x01)
     * bit 0 = 1: 8086 mode (as opposed to ancient 8080 mode) */
    outb(0x21, 0x01);
    io_wait();
    outb(0xA1, 0x01);
    io_wait();

    // Restore the saved masks 
    outb(0x21, mask_master);
    outb(0xA1, mask_slave);
}

/*
 * Register a C function to handle a specific interrupt number.
 * For example, the keyboard driver will call:
 *   idt_register_handler(33, keyboard_handler);
 * because IRQ 1 (keyboard) is mapped to interrupt 33 after PIC remap.
 */
void idt_register_handler(uint8_t interrupt, interrupt_handler_t handler) {
    handlers[interrupt] = handler;
}

/*
 * The common C interrupt handler — called by ALL interrupt asm stubs.
 * It looks up the right handler in our array and calls it.
 *
 * For hardware IRQs (32-47), we must also send an "End of Interrupt" (EOI)
 * signal to the PIC, or it won't send us any more interrupts.
 */
void interrupt_handler(struct interrupt_frame *frame) {
    // If someone registered a handler for this interrupt, call it 
    if (handlers[frame->interrupt_number] != NULL) {
        handlers[frame->interrupt_number](frame);
    }

    /* Send EOI (End of Interrupt) to the PIC for hardware IRQs.
     * IRQs are interrupts 32-47 (after our remap).
     * If the IRQ came from the slave PIC (40-47), we must send
     * EOI to BOTH the slave AND the master.
     * If it came from the master (32-39), only send to master.
     *
     * Note: IRQ 0 (timer) also sends EOI in timer_handler before
     * schedule(), resulting in a double EOI. This is harmless
     * (ISR is already clear) but the early EOI is necessary because
     * context_switch may not return to this stack frame. */
    if (frame->interrupt_number >= 32 && frame->interrupt_number <= 47) {
        if (frame->interrupt_number >= 40) {
            outb(0xA0, 0x20);  // EOI to slave PIC
        }
        outb(0x20, 0x20);     // EOI to master PIC
    }
}

void idt_init(void) {
    // Remap the PIC first, before we install any IRQ handlers 
    pic_remap();

    // Tell the CPU the size and location of our IDT 
    idtp.limit = (sizeof(struct idt_entry) * IDT_ENTRIES) - 1;
    idtp.base  = (uint32_t)&idt;

    /* Install CPU exception handlers (0-31)
     * Flags: 0x8E = 1000 1110
     *   bit 7    = 1 : Present (this entry is valid)
     *   bits 5-6 = 00: Ring 0 (only kernel can trigger via INT instruction)
     *   bit 4    = 0 : Always 0 for interrupt gates
     *   bits 0-3 = 1110: 32-bit interrupt gate
     *
     * "Interrupt gate" means the CPU automatically disables interrupts
     * (clears IF flag) when this handler is entered. This prevents an
     * interrupt from interrupting another interrupt handler.
     */
    idt_set_entry(0,  (uint32_t)isr0,  0x08, 0x8E);
    idt_set_entry(1,  (uint32_t)isr1,  0x08, 0x8E);
    idt_set_entry(2,  (uint32_t)isr2,  0x08, 0x8E);
    idt_set_entry(3,  (uint32_t)isr3,  0x08, 0x8E);
    idt_set_entry(4,  (uint32_t)isr4,  0x08, 0x8E);
    idt_set_entry(5,  (uint32_t)isr5,  0x08, 0x8E);
    idt_set_entry(6,  (uint32_t)isr6,  0x08, 0x8E);
    idt_set_entry(7,  (uint32_t)isr7,  0x08, 0x8E);
    idt_set_entry(8,  (uint32_t)isr8,  0x08, 0x8E);
    idt_set_entry(9,  (uint32_t)isr9,  0x08, 0x8E);
    idt_set_entry(10, (uint32_t)isr10, 0x08, 0x8E);
    idt_set_entry(11, (uint32_t)isr11, 0x08, 0x8E);
    idt_set_entry(12, (uint32_t)isr12, 0x08, 0x8E);
    idt_set_entry(13, (uint32_t)isr13, 0x08, 0x8E);
    idt_set_entry(14, (uint32_t)isr14, 0x08, 0x8E);
    idt_set_entry(15, (uint32_t)isr15, 0x08, 0x8E);
    idt_set_entry(16, (uint32_t)isr16, 0x08, 0x8E);
    idt_set_entry(17, (uint32_t)isr17, 0x08, 0x8E);
    idt_set_entry(18, (uint32_t)isr18, 0x08, 0x8E);
    idt_set_entry(19, (uint32_t)isr19, 0x08, 0x8E);
    idt_set_entry(20, (uint32_t)isr20, 0x08, 0x8E);
    idt_set_entry(21, (uint32_t)isr21, 0x08, 0x8E);
    idt_set_entry(22, (uint32_t)isr22, 0x08, 0x8E);
    idt_set_entry(23, (uint32_t)isr23, 0x08, 0x8E);
    idt_set_entry(24, (uint32_t)isr24, 0x08, 0x8E);
    idt_set_entry(25, (uint32_t)isr25, 0x08, 0x8E);
    idt_set_entry(26, (uint32_t)isr26, 0x08, 0x8E);
    idt_set_entry(27, (uint32_t)isr27, 0x08, 0x8E);
    idt_set_entry(28, (uint32_t)isr28, 0x08, 0x8E);
    idt_set_entry(29, (uint32_t)isr29, 0x08, 0x8E);
    idt_set_entry(30, (uint32_t)isr30, 0x08, 0x8E);
    idt_set_entry(31, (uint32_t)isr31, 0x08, 0x8E);

    // Install hardware IRQ handlers (32-47) 
    idt_set_entry(32, (uint32_t)irq0,  0x08, 0x8E);
    idt_set_entry(33, (uint32_t)irq1,  0x08, 0x8E);
    idt_set_entry(34, (uint32_t)irq2,  0x08, 0x8E);
    idt_set_entry(35, (uint32_t)irq3,  0x08, 0x8E);
    idt_set_entry(36, (uint32_t)irq4,  0x08, 0x8E);
    idt_set_entry(37, (uint32_t)irq5,  0x08, 0x8E);
    idt_set_entry(38, (uint32_t)irq6,  0x08, 0x8E);
    idt_set_entry(39, (uint32_t)irq7,  0x08, 0x8E);
    idt_set_entry(40, (uint32_t)irq8,  0x08, 0x8E);
    idt_set_entry(41, (uint32_t)irq9,  0x08, 0x8E);
    idt_set_entry(42, (uint32_t)irq10, 0x08, 0x8E);
    idt_set_entry(43, (uint32_t)irq11, 0x08, 0x8E);
    idt_set_entry(44, (uint32_t)irq12, 0x08, 0x8E);
    idt_set_entry(45, (uint32_t)irq13, 0x08, 0x8E);
    idt_set_entry(46, (uint32_t)irq14, 0x08, 0x8E);
    idt_set_entry(47, (uint32_t)irq15, 0x08, 0x8E);

    // syscall gate (INT 0x80)
    // 0xEE = 1110 1110 = present, DPL=3, 32-bit interrupt gate
    // DPL=3 is the key part here. all the other IDT entries use 0x8E (DPL=0)
    // which means only ring 0 code can invoke them via INT instruction
    // but user mode code is ring 3, so without DPL=3 here a user INT 0x80
    // would GPF instead of reaching our syscall handler
    // the CPU checks: CPL <= DPL, ring 3 needs DPL >= 3
    // still an interrupt gate (not trap), so IF gets cleared on entry
    // which prevents the timer from preempting us mid-syscall
    idt_set_entry(128, (uint32_t)isr128, 0x08, 0xEE);

    // Load the IDT into the CPU, same idea as lgdt but for interrupts
    __asm__ volatile ("lidt %0" : : "m"(idtp));

    /* Enable interrupts! (set the IF flag in EFLAGS)
     * Up until now, interrupts have been disabled since boot.
     * This single instruction turns them on — the keyboard will
     * start firing IRQ1 as soon as a key is pressed. */
    __asm__ volatile ("sti");
}
