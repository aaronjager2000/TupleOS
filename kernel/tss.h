#ifndef TSS_H
#define TSS_H

#include <stdint.h>

// Task State Segment

// The TSS is an x86 hardware structure that the CPU consults during privilege level transitions (Ring 3 -> Ring 0)

// When a user mode program gets interrupted, the CPU needs a safe kernel stack to switch to. It finds that stack by reading ESP0 and SSO from the TSS. Without a TSS, the cpu has no idea where the kernel stack is
// and a ring 3 -> ring - transition would triple fault

// We do NOT use hardware task switching (one TSS per process)
// instead, we have ONE TSS for the whole system, and we update ESP0 on every context switch to point to the current process's kernel stack. this is what linux and every modern OS does

// Most fields in this struct are leftovers from Intel's hardware task switching design. We only care about:
//  esp0: kernel stack pointer (updated on every context switch)
//  ss0: kernel stack segment (always 0x10, our kernel data segment)
//  iomap_base: set to sizeof(tss) to mean "no I/O permission bitmap"

typedef struct {
    uint32_t prev_tss; // Link to previous TSS (hardware task chaining, unused)
    uint32_t esp0; // Stack pointer for ring 0 THE critical field
    uint32_t ss0; // Stack segment for Ring 0 (always 0x10)
    uint32_t esp1; // Ring 1 stack pointer (unused, why tf would anyone use ring 1)
    uint32_t ss1; // Ring 1 stack segment (unused)
    uint32_t esp2; // Ring 2 stack pointer (unused)
    uint32_t ss2; // Ring 2 stack segment (unused)
    uint32_t cr3; // page dir base (hardware task switch, unused)
    uint32_t eip; // Instruction pointer (hardware task switch, unused)
    uint32_t eflags; // Flags register (hardware task switch, unused)
    uint32_t eax; // gen purpose registers (all unused by us)
    uint32_t ecx;
    uint32_t edx; 
    uint32_t ebx; 
    uint32_t esp; 
    uint32_t ebp; 
    uint32_t esi; 
    uint32_t edi; 
    uint32_t es; // segment selectors (all unused by us) 
    uint32_t cs; 
    uint32_t ss; 
    uint32_t ds; 
    uint32_t fs; 
    uint32_t gs; 
    uint32_t ldt; // LDT selector (unused, we don't use an LDT)
    uint16_t trap; // debug trap flag (unused)
    uint16_t iomap_base; // Offset to I/O permission bitmap (set to sizeof(tss)) 
} __attribute__((packed)) tss_t;

// must be called AFTER gdt_init() sets up the TSS descriptor
void tss_init(uint32_t kernel_stack_top);

// update ESP0 in the TSS, will be called on every context switch
// so that the CPU knows where teh new process's kenrel stack is
void tss_set_kernel_stack(uint32_t stack_top);

tss_t* tss_get(void);

#endif