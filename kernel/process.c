#include "process.h"
#include "kheap.h"
#include "paging.h"
#include "kprintf.h"
#include "scheduler.h"

//asm func
extern void kthread_trampoline(void);

// The process table
// A flat array of PCBs, not pointers. Slots state with state = PROCESS_UNUSED are free.
// We can linearly to find free slots, this is O(n) but n is 64 so who cares

// PIDsa are NOT the same as table indices. PIDs increment forever and never wrap.
// A PID of 73 mihgt live in slot 2 if earlier processes exited and freed slots.
// This matters because reusing PIDs would let a process accidentally signal/wait on the wrong target

static process_t process_table[MAX_PROCESSES];

// Monotonically increasing PID counter. PID 0 is reserved for the kernel.
static uint32_t next_pid = 0;

// points to whichever process_t is currently RUNNING on the CPU
// the scheduler updates this on every context switch
static process_t* current_process = NULL;

// Zero out a block of memory. We don't have libc's memset in freestanding mode, so we do it manually
// If we add memset we can swap this out but that's cheating IMO
static void zero_memory(void* dest, uint32_t size) {
    uint8_t* d = (uint8_t*)dest;
    for (uint32_t i = 0; i < size; i++) {
        d[i] = 0;
    }
}

// Copy a string into a fixed-size buffer, always null terminates
// Like strncpy but without the annoying "doesn't null terminate" footgun

static void copy_name(char* dest, const char* src, uint32_t max_len) {
    uint32_t i;
    for (i = 0; i < max_len - 1 && src[i] != '\0'; i++) {
        dest[i] = src[i];
    }
    dest[i] = '\0';
}

void process_init(void) {
    // clear the entire process table, all slots start as UNUSED
    zero_memory(process_table, sizeof(process_table));

    // set up PID 0: the kernel "process"

    // this is lowkey a bit of fiction cause the kernel was already running before process_init
    // it booted, set up paging, the heap, interrupts, everything
    // PID 0 just wraps that existing execution context into the process framework so the scheduler can treat it uniformly

    // PID 0 doesn't need:
    //  A separately allocaetd kernel stack (it uses the boot stack from boot.asm)
    //  A separately created address space (it IS the kernel address space)

    // kernel_esp is 0 for now. It gets filled in the first time the scheduler switches AWAY from PID 0, at which point the timer ISR's stack frame will be sitting on the boot stack, and we save that ESP

    process_t* kernel_proc = &process_table[0];
    kernel_proc->pid = 0;
    kernel_proc->state = PROCESS_RUNNING;
    copy_name(kernel_proc->name, "kernel", PROCESS_NAME_LEN);

    kernel_proc->page_directory = paging_get_directory();
    kernel_proc->address_space = vmm_get_kernel_space();
    kernel_proc->kernel_stack = 0; // boot stack, not heap allocated
    kernel_proc->kernel_esp = 0; // set on first switch away
    kernel_proc->priority = 0; // highest priority (for obvious reasons)
    kernel_proc->parent_pid = 0; // kernel is its own parent
    kernel_proc->exit_code = 0;

    next_pid = 1;
    current_process = kernel_proc;

    kprintf("[PROCESS] Subsystem initialized, PID 0 (kernel) active\n");
}

process_t* process_alloc(void) {
    // linear scan for free slot
    int slot = -1;
    for (int i = 0; i < MAX_PROCESSES; i++) {
        if (process_table[i].state == PROCESS_UNUSED) {
            slot = i;
            break;
        }
    }

    if (slot == -1) {
        kprintf("[PROCESS] Table full! (max %d)\n", MAX_PROCESSES);
        return NULL;
    }

    process_t* proc = &process_table[slot];

    // Zero the entire PCB so we start clean
    zero_memory(proc, sizeof(process_t));

    // Assign a unique PID (monotonically increasing, never reused)
    proc->pid = next_pid++;
    proc->state = PROCESS_READY;
    proc->parent_pid = current_process ? current_process->pid : 0;

    // allocate a kernel stack for this process
    // Every process MUST have its own kernel stack

    // when a user mode process gets interrupted (timer tick, syscall, etc.)
    // the CPU switches to ring 0 and starts pushing state onto the kernel stack
    // if two processes shared a kernel staack, the second interrupt would thrash the first process's save state
    // separate stacks keep them isolated

    // we use kmalloc_aligned to get a page aligned 8KB block
    // The x86 stack grows DOWNARD, so kernel_esp starts at the TOP

    // kernel_stack (base)              kernel_esp (initial, top)
    //  |                                |
    //  V                                V
    //  [      8192 bytes of stack       ]
    //  low addr ------------------> high addr
    
    // as the process takes interrupts, kernel_esp moves left
    proc->kernel_stack = (uint32_t)kmalloc_aligned(KERNEL_STACK_SIZE);
    if (!proc->kernel_stack) {
        kprintf("[PROCESS] Failed to allocate kernel stack for PID %u\n", proc->pid);
        proc->state = PROCESS_UNUSED;
        return NULL;
    }

    proc->kernel_esp = proc->kernel_stack + KERNEL_STACK_SIZE;

    kprintf("[PROCESS] Allocated PID %u (slot %d, kstack @ 0x%x)\n", proc->pid, slot, proc->kernel_stack);
    return proc;
}

void process_free(process_t* proc) {
    if (!proc) return;

    // never free the kernel process
    if (proc->pid == 0) {
        kprintf("[PROCESS] BUG: Tried to free PID 0 (kernel)!\n");
        return;
    }

    uint32_t pid = proc->pid;
    char name_copy[PROCESS_NAME_LEN];
    copy_name(name_copy, proc->name, PROCESS_NAME_LEN);

    // Free the kernel stack (was allocated with kmalloc_aligned)
    if (proc->kernel_stack) {
        kfree((void*)proc->kernel_stack);
    }

    // Destroy the process's address space (unmaps user regions, free page dir)
    // The kernel regions are left alone, vmm_destroy_address_space handles that
    if (proc->address_space) {
        vmm_destroy_address_space(proc->address_space);
    }

    // Clear and mark the slot as free
    zero_memory(proc, sizeof(process_t));
    proc->state = PROCESS_UNUSED;

    kprintf("[PROCESS] Freed PID %u (%s)\n", pid, name_copy);
}

process_t* process_get(uint32_t pid) {
    for (int i = 0; i < MAX_PROCESSES; i++) {
        if (process_table[i].state != PROCESS_UNUSED && process_table[i].pid == pid) {
            return &process_table[i];
        }
    }
    return NULL;
}

process_t* process_current(void) {
    return current_process;
}

process_t* process_get_by_slot(int slot) {
    if (slot < 0 || slot >= MAX_PROCESSES) return NULL;
    if (process_table[slot].state == PROCESS_UNUSED) return NULL;
    return &process_table[slot];
}

void process_set_current(process_t* proc) {
    current_process = proc;
}

void kthread_exit(void) {
    // disable interruipts cause we're abt to modify process state and call
    // schedule() directly (not from a timer ISR), so we need atomicity
    __asm__ volatile("cli");

    process_t* proc = process_current();
    if (proc) {
        kprintf("[KTHREAD] '%s' (PID %u) exited\n", proc->name, proc->pid);
        proc->state = PROCESS_ZOMBIE;
    }

    schedule();

    while (1) { __asm__ volatile("hlt"); }
}

// create a kernel thread

// allocates a PCB and kernel stack, then sets up the stack so taht when context_switch() switches to this thread for the first time, it "returns" into kthread_trampoline, which enables interrupts and calls the entry function
// the trick: context_switch() does pop edi/esi/ebx/ebp then ret.
// we build a fake stack frame that matches what context_switch expects to pop, wioth the entry function pointer in the EBX slot and kthread_trampoline as the return address

// Init kernel stack layout (low addr at top)

// EDI = 0 <-- kernel_esp (context_switch starts popping here)
// ESI = 0
// EBX = entry function ptr (kthread_trampoline reads this and calls it)
// EBP = 0
// kthread_trampoline (context_switch's ret jumps here)
// 0 (dummy) trampoline's "return address" never used

process_t* kthread_create(void (*entry)(void), const char* name) {
    process_t* proc = process_alloc();
    if (!proc) return NULL;

    copy_name(proc->name, name, PROCESS_NAME_LEN);

    // kernel threads share the kernel's address space
    proc->page_directory = paging_get_directory();
    proc->address_space = vmm_get_kernel_space();

    // build the fake stack frame
    uint32_t* sp = (uint32_t*)(proc->kernel_stack + KERNEL_STACK_SIZE);

    *(--sp) = 0; // dummy return addr for trampoline
    *(--sp) = (uint32_t)kthread_trampoline; // context_switch ret -> here
    *(--sp) = 0; // EBP
    *(--sp) = (uint32_t)entry; // EBX = entry function
    *(--sp) = 0; // ESI
    *(--sp) = 0;

    proc->kernel_esp = (uint32_t)sp;

    kprintf("[KTHREAD] Created '%s' (PID %u, entry @ 0x%x)\n", proc->name, proc->pid, (uint32_t)entry);
    return proc;
}



