#ifndef IDT_H
#define IDT_H

#include <stdint.h>

/*
 * idt.h - Interrupt Descriptor Table
 *
 * The IDT maps interrupt numbers (0-255) to handler functions.
 *
 * There are 3 categories of interrupts:
 *   0-31:   CPU exceptions (divide by zero, page fault, etc.)
 *            These are hardwired by Intel — you can't change what
 *            triggers them, only what function handles them.
 *   32-47:  Hardware IRQs (timer, keyboard, disk, etc.)
 *            We remap these here because by default the PIC maps
 *            IRQs 0-7 to interrupts 8-15, which COLLIDES with
 *            CPU exceptions. So we move them to 32-47.
 *   48-255: Available for software interrupts (like syscalls later)
 *
 * Each IDT entry is 8 bytes, similar messy layout to the GDT.
 */

 // Total number of IDT entries. 256 is the max the CPU supports
 #define IDT_ENTRIES 256

 /* A single IDT entry (8 bytes)
 *
 * Like the GDT, the handler address is split into two halves
 * because Intel kept backwards compatibility with the 286.
 */
 struct idt_entry {
    uint16_t base_low; // Lower 16 bits of handler function address
    uint16_t selector; // Kernel code segment selector (0x08 from our GDT)
    uint8_t zero; // Always 0. Reserved by CPU
    uint8_t flags; // Type and attributes (present, privilige, gate type)
    uint16_t base_high; // Upper 16 bits of handler function address
 } __attribute__((packed));

 // Same idea as GDT pointer, the `lidt` instruction reads this to find our table
 struct idt_ptr {
    uint16_t limit; // Size of IDT in bytes minus 1
    uint32_t base; // Memory address of the IDT
 } __attribute__((packed));

 /* Struct passed to our C handler when an interrupt fires.
 * The assembly stubs (in interrupts.asm) push all this onto the
 * stack before calling our C function, so we can inspect the
 * full CPU state at the moment the interrupt happened.
 */
 struct interrupt_frame {
    // Registers we push manually in our asm stub (pusha)
    uint32_t ds; // Data segment we were using
    uint32_t edit, esi, ebp, esp, ebx, edx, ecx, eax; // General-purpsoe registers (from pusha)

    // Pushed by our asm stub before pusha
    uint32_t interrupt_number; // Which interrupt fired (0-255)
    uint32_t error_code; // Error code (CPU pushes for some exceptions, we push 0 for others)

    // Pushed automatically by the CPU when the interrupt fires
    uint32_t eip; // Instruction pointer, where the code was executing
    uint32_t cs; // Code segment at time of interrupt
    uint32_t eflags; // CPU flags at time of interrupt
    uint32_t user_esp; // Stack pointer (only valid if privilege level changed)
    uint32_t user_ss; // Stack segment (same as above, only valid if privilege level changed)
 };

 // Set up and install the IDT, called once during boot
 void idt_init(void);

 // Register a C function as the handler for a specific interrupt number, this is how keyboard.c will say "call my function when IRQ 1 fires"
 typedef void (*interrupt_handler_t)(struct interrupt_frame *frame);
 void idt_register_handler(uint8_t interrupt, interrupt_handler_t handler);

 #endif