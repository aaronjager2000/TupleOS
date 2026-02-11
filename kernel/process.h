#ifndef PROCESS_H
#define PROCESS_H

#include <stdint.h>
#include "vmm.h"

// Process Control Block (PCB)

// This is the core data structure that represents a process. Every entity that can be scheduled to run the CPU gets one of these. It holds everything the kernel needs to pause a process, run something else, and come back later as if nothing happened.
// Right now we're just defining the container, no scheduling, no context switching, no user mode. This is the foundation that steps 2-5 build on.

#define MAX_PROCESSES 64
#define PROCESS_NAME_LEN 32
#define KERNEL_STACK_SIZE 8192 // 8KB per process kernel stack

// process lifecycle states
typedef enum {
    PROCESS_UNUSED = 0, // PCB slot is available in the process table
    PROCESS_READY, // runnable, waiting in the ready queue for the scheduler to pick it up
    PROCESS_RUNNING, // currently executing on the CPU (only one at a time on uniprocessor)
    PROCESS_BLOCKED, // waiting on something; I/O, sleep, semaphone, etc.
    PROCESS_ZOMBIE, // process exited, but parent hasn't collected exit_code yet (waitpid)
} process_state_t;

// Saved CPU state for a process

// This struct captures the complete CPU register state needed to resume a process
// It's used for 2 purposes:
// 1. Setting up the initial register state when creating a new process
// 2. Documentation of what gets saved/restored during a context switch

// the ACTUAL mechanism for context switching is kernel stack swapping:
// the timer ISR saves the full interrupt_frame onto the current process's kernel stack, we swap kernel_esp to point at the new process's kernel stack, and the ISR restore path + iret loads the new process's state
// so the real saved state lives ON the kernel stack, not copied into this struct on every switch

// Register order matches pusha convention for consistency with interrupt_frame

typedef struct {
    // General-purpose registers (pusha order, lowest addr first)
    uint32_t edi, esi, ebp, esp, ebx, edx, ecx, eax;

    // segment selectors
    uint32_t ds, es, fs, gs;

    // Instruction pointer, code segment, flags
    uint32_t eip;
    uint32_t cs;
    uint32_t eflags;

    // user mode stack info (only meaningful for ring 3 processes naturally)
    // the CPU pushes/pops these automatically on privilege level changes
    uint32_t user_esp;
    uint32_t user_ss;
} cpu_registers_t;

// The PCB itself
// One of these exists for every active process in the system
// The process table is a flat array of these, not pointers to heap allocated structs
// This avoids chicken and egg issues with the heap and keeps things cache friendly
typedef struct {
    uint32_t pid; // unique, monotonically increasing, never reused
    process_state_t state; // curr lifecycle state
    char name[PROCESS_NAME_LEN]; // human readable name for debugging

    cpu_registers_t registers; // saved CPU state (used for init setup)

    uint32_t* page_directory; // physical addr for CR3 (this process's view of memory)
    vmm_address_space_t* address_space; // VMM wrapper; regions + page dir

    uint32_t kernel_stack; // base address of this process's kernel stack allocation
    uint32_t kernel_esp; // saved kernel ESP; THIS is the context switch pivot point

    uint32_t priority; // for future scheduling (0 = highest)
    uint32_t parent_pid; // who spwaned this process
    int32_t exit_code; // set on exit, read by parent via waitpid
} process_t; 

// init process subsystem, sets up PID 0 for the kernel
void process_init(void);

// Allocate a new PCB from the process table. Returns NULL if table is full
// Assigns a PID, allocates a kernel stack, sets state to READY
process_t* process_alloc(void);

// Free a PCB slot. Releases kernel stack, destroys address space, marks slot UNUSED
void process_free(process_t* proc);

// Look up a process by PID. Returns NULL if not found or slot is UNUSED
process_t* process_get(uint32_t pid);

// get the currently running process (the one whose kernel stack we're on)
process_t* process_current(void);

// access a process by table slot index (0 to MAX_PROCESSES-1)
process_t* process_get_by_slot(int slot);

// update the current process pointer (called by scheduler during context switch)
void process_set_current(process_t* proc);

// create a kernel thread that executes entry() in ring 0, the thread shares the kernel's addr space (same page dir)
// return new process, or NULL on failure
process_t* kthread_create(void (*entry)(void), const char* name);

// Marks process as ZOMBIE and yields to scheduler
void kthread_exit(void);

#endif