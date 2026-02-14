#include "tss.h"
#include "kprintf.h"

// The one and only TSS for the entire system

// We zero-init it, then fill in the 3 fields we care about
// This struct lives in kernel BSS, so it's always at a fixed virtual addr
// that we can point the GDT TSS descriptor at

static tss_t tss;

// defined in gdt_flush.asm, exectures the `ltr` instruction
extern void tss_flush(uint16_t tss_selector);

void tss_init(uint32_t kernel_stack_top) {
    // zero the whole struct first, all of the hardware task switching fields we don't use should be 0
    for (uint32_t i = 0; i < sizeof(tss); i++) {
        ((uint8_t*)&tss)[i] = 0;
    }

    // SS0 is the kernel data segment selector
    // when the CPU transitions ring 3 -> ring 0, it loads this into SS
    tss.ss0 = 0x10;

    // kernel stack pointer
    tss.esp0 = kernel_stack_top;


    // IOMAP_BASE: offset to the I/O permission bitmap
    // setting this to sizeof(tss_t) means "the bitmap starts right after the struct", but since we set the TSS limit to sizeof(tss)-1 there IS NO bitmap
    // this effectively means "no I/O ports allowed from ring 3" which is what we want (user programs shouldnt do port I/O)
    tss.iomap_base = sizeof(tss_t);


    // apparently we need to load the task register with the TSS selector, 0x28 = GDT entry 5 which is the ltr instruction that tellsthe CPU the tss lives at this entry or something I honestly dont understand this part but it works so dont change it
    // after this the cpu will automatically read ESP0/SS0 from our tss struct on every ring 3 -> ring 0 transition
    tss_flush(0x28);

    kprintf("[TSS] Initialized, esp0=0x%x\n", kernel_stack_top);
}

void tss_set_kernel_stack(uint32_t stack_top) {
    tss.esp0 = stack_top;
}

tss_t* tss_get(void) {
    return &tss;
}

