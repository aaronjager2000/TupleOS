#include "process.h"
#include "kheap.h"
#include "paging.h"
#include "kprintf.h"
#include "scheduler.h"

//asm func
extern void kthread_trampoline(void);

extern void usermode_trampoline(void);

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

// copy n bytes from src to dest. same deal as zero_memory, we roll our own because freestanding means no libc memcpy
static void copy_memory(void* dest, const void* src, uint32_t size) {
    uint8_t* d = (uint8_t*)dest;
    const uint8_t* s = (const uint8_t*)src;
    for (uint32_t i = 0; i < size; i++) {
        d[i] = s[i];
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

static int name_equals(const char* a, const char* b) {
    while (*a && *b) {
        if (*a++ != *b++) return 0;
    }
    return *a == *b;
}

// ============================================================
// built-in test programs (raw i386 machine code)
// these get copied into user space when spawned via the shell
// ============================================================

// hello - prints "Hello from user mode!" and exits cleanly
//
//   mov $1, %ebx           ; fd = stdout
//   mov $string, %ecx      ; buf = embedded string at offset 0x24
//   mov $22, %edx          ; len
//   mov $1, %eax           ; SYS_WRITE
//   int $0x80
//   mov $0, %ebx           ; exit code 0
//   mov $0, %eax           ; SYS_EXIT
//   int $0x80
//   jmp $                   ; safety net
//   db "Hello from user mode!\n"
static const uint8_t prog_hello[] = {
    0xBB, 0x01, 0x00, 0x00, 0x00,
    0xB9, 0x24, 0x00, 0x40, 0x00,
    0xBA, 0x16, 0x00, 0x00, 0x00,
    0xB8, 0x01, 0x00, 0x00, 0x00,
    0xCD, 0x80,
    0xBB, 0x00, 0x00, 0x00, 0x00,
    0xB8, 0x00, 0x00, 0x00, 0x00,
    0xCD, 0x80,
    0xEB, 0xFE,
    0x48, 0x65, 0x6C, 0x6C, 0x6F, 0x20, 0x66, 0x72,
    0x6F, 0x6D, 0x20, 0x75, 0x73, 0x65, 0x72, 0x20,
    0x6D, 0x6F, 0x64, 0x65, 0x21, 0x0A
};

// counter - prints digits 1-0 repeating, 100 times total, yielding between each
// proves cooperative multitasking works. output looks like: 1234567890123456789012...
//
//   mov $100, %esi          ; 100 iterations
//   xor %edi, %edi          ; digit index 0-9
// loop:
//   mov $1, %ebx
//   mov $digits, %ecx       ; "1234567890" at offset 0x4F
//   add %edi, %ecx          ; index into it
//   mov $1, %edx
//   mov $1, %eax            ; SYS_WRITE
//   int $0x80
//   mov $3, %eax            ; SYS_YIELD
//   int $0x80
//   inc %edi
//   cmp $10, %edi
//   jne no_wrap
//   xor %edi, %edi
// no_wrap:
//   dec %esi
//   jnz loop
//   <print newline, exit(0)>
static const uint8_t prog_counter[] = {
    0xBE, 0x64, 0x00, 0x00, 0x00,
    0x31, 0xFF,
    0xBB, 0x01, 0x00, 0x00, 0x00,
    0xB9, 0x4F, 0x00, 0x40, 0x00,
    0x01, 0xF9,
    0xBA, 0x01, 0x00, 0x00, 0x00,
    0xB8, 0x01, 0x00, 0x00, 0x00,
    0xCD, 0x80,
    0xB8, 0x03, 0x00, 0x00, 0x00,
    0xCD, 0x80,
    0x47,
    0x83, 0xFF, 0x0A,
    0x75, 0x02,
    0x31, 0xFF,
    0x4E,
    0x75, 0xD6,
    // done, print newline
    0xBB, 0x01, 0x00, 0x00, 0x00,
    0xB9, 0x59, 0x00, 0x40, 0x00,
    0xBA, 0x01, 0x00, 0x00, 0x00,
    0xB8, 0x01, 0x00, 0x00, 0x00,
    0xCD, 0x80,
    0x31, 0xDB,
    0x31, 0xC0,
    0xCD, 0x80,
    0xEB, 0xFE,
    // "1234567890\n"
    0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x30,
    0x0A
};

// spinner - infinite loop printing '*' with no yield
// if preemption works, other processes still get CPU time
// if preemption is broken, this starves everything
//
// loop:
//   mov $1, %ebx
//   mov $star, %ecx         ; '*' char at offset 0x18
//   mov $1, %edx
//   mov $1, %eax            ; SYS_WRITE
//   int $0x80
//   jmp loop
static const uint8_t prog_spinner[] = {
    0xBB, 0x01, 0x00, 0x00, 0x00,
    0xB9, 0x18, 0x00, 0x40, 0x00,
    0xBA, 0x01, 0x00, 0x00, 0x00,
    0xB8, 0x01, 0x00, 0x00, 0x00,
    0xCD, 0x80,
    0xEB, 0xE8,
    0x2A
};

// syscall_test - exercises every implemented syscall then exits
// getpid -> write "OK\n" -> yield -> sleep(100ms) -> write "OK\n" -> exit(0)
//
//   mov $2, %eax            ; SYS_GETPID
//   int $0x80
//   <write "OK\n">
//   mov $3, %eax            ; SYS_YIELD
//   int $0x80
//   mov $100, %ebx          ; 100ms
//   mov $4, %eax            ; SYS_SLEEP
//   int $0x80
//   <write "OK\n">
//   mov $0, %ebx            ; SYS_EXIT(0)
//   mov $0, %eax
//   int $0x80
static const uint8_t prog_syscall_test[] = {
    0xB8, 0x02, 0x00, 0x00, 0x00,
    0xCD, 0x80,
    0xBB, 0x01, 0x00, 0x00, 0x00,
    0xB9, 0x54, 0x00, 0x40, 0x00,
    0xBA, 0x03, 0x00, 0x00, 0x00,
    0xB8, 0x01, 0x00, 0x00, 0x00,
    0xCD, 0x80,
    0xB8, 0x03, 0x00, 0x00, 0x00,
    0xCD, 0x80,
    0xBB, 0x64, 0x00, 0x00, 0x00,
    0xB8, 0x04, 0x00, 0x00, 0x00,
    0xCD, 0x80,
    0xBB, 0x01, 0x00, 0x00, 0x00,
    0xB9, 0x54, 0x00, 0x40, 0x00,
    0xBA, 0x03, 0x00, 0x00, 0x00,
    0xB8, 0x01, 0x00, 0x00, 0x00,
    0xCD, 0x80,
    0xBB, 0x00, 0x00, 0x00, 0x00,
    0xB8, 0x00, 0x00, 0x00, 0x00,
    0xCD, 0x80,
    0xEB, 0xFE,
    0x4F, 0x4B, 0x0A
};

// registry so the shell can look up programs by name
typedef struct {
    const char* name;
    const uint8_t* code;
    uint32_t size;
} test_program_t;

static const test_program_t test_programs[] = {
    { "hello",        prog_hello,        sizeof(prog_hello) },
    { "counter",      prog_counter,      sizeof(prog_counter) },
    { "spinner",      prog_spinner,      sizeof(prog_spinner) },
    { "syscall_test", prog_syscall_test, sizeof(prog_syscall_test) },
};

#define NUM_TEST_PROGRAMS (sizeof(test_programs) / sizeof(test_programs[0]))

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
    // DON'T set READY here. the caller sets READY after the kernel stack and
    // address space are fully initialized. otherwise a timer interrupt can
    // context-switch to this process before its stack is set up -> crash
    proc->state = PROCESS_BLOCKED;
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
      kfree_aligned((void*)proc->kernel_stack);
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
    proc->state = PROCESS_READY;

    kprintf("[KTHREAD] Created '%s' (PID %u, entry @ 0x%x)\n", proc->name, proc->pid, (uint32_t)entry);
    return proc;
}

// create a user mode process that runs in ring 3
// this is the ring 3 counterpart to kthread_create(), instead of sharing the kernel's address space, the new process gets its own page dir with user accessible code and stack regions mapped in


// we set up the kernal stack layout so when context_switch() switches to this process for the first time, it returns into usermode_trampoline which will find an iret frame waiting on the stack and does iret to drop into ring 3

// after that first iret, this kernel stack is empty
// apparently we need that because the TSS has ESP0 pointing to the top of this stack, so when the next interrupt fires in ring 3, the CPU starts pushing user state from the top

process_t* uprocess_create(const uint8_t* code, uint32_t code_size, const char* name) {
    if (!code || code_size == 0 || code_size > PAGE_SIZE) {
        kprintf("[UPROCESS] Invalid code (size=%u, max=%u)\n", code_size, PAGE_SIZE);
        return NULL;
    }

    process_t* proc = process_alloc();
    if (!proc) return NULL;

    copy_name(proc->name, name, PROCESS_NAME_LEN);

    // create a new addr space
    // temporarily switch to new addr space to map use pages
    // map user code reagion at USER_CODE_BASE
    // copy the program's machine code into the mapped user code page
    // map user stack
    // switch back to the kernel's addr space
    // build the kernel stack

    proc->address_space = vmm_create_address_space();
    if (!proc->address_space) {
        kprintf("[UPROCESS] Failed to create address space\n");
        process_free(proc);
        return NULL;
    }
    proc->page_directory = (uint32_t*)proc->address_space->page_directory;

    // save interrupt state and disable. we use pushfl/popfl instead of cli/sti
    // because we might already be inside an ISR (keyboard handler) where IF is
    // already off. a bare sti would enable interrupts inside the ISR which is bad.
    __asm__ volatile("pushfl; cli" ::: "memory");

    vmm_address_space_t* kernel_space = vmm_get_kernel_space();
    vmm_switch_address_space(proc->address_space);

    int ret = vmm_map_region(proc->address_space, USER_CODE_BASE, PAGE_SIZE, VMM_READ | VMM_WRITE | VMM_EXEC | VMM_USER, REGION_USER_CODE);
    if (ret != 0) {
        kprintf("[UPROCESS] Failed to map user code region\n");
        vmm_switch_address_space(kernel_space);
        __asm__ volatile("popfl" ::: "memory");
        process_free(proc);
        return NULL;
    }

    copy_memory((void*)USER_CODE_BASE, code, code_size);

    ret = vmm_map_region(proc->address_space, USER_STACK_PAGE, PAGE_SIZE, VMM_READ | VMM_WRITE | VMM_USER, REGION_USER_STACK);
    if (ret != 0) {
        kprintf("[UPROCESS] Failed to map user stack region\n");
        vmm_switch_address_space(kernel_space);
        __asm__ volatile("popfl" ::: "memory");
        process_free(proc);
        return NULL;
    }

    vmm_switch_address_space(kernel_space);

    // build the kernel stack BEFORE enabling interrupts or marking READY
    uint32_t* sp = (uint32_t*)(proc->kernel_stack + KERNEL_STACK_SIZE);

    *(--sp) = 0x23; // SS: user data segment
    *(--sp) = USER_STACK_TOP; // ESP: top of user stack
    *(--sp) = 0x202; // EFLAGS: IF=1 (interrupts on), bit 1 always set
    *(--sp) = 0x1B; // CS: user code segment
    *(--sp) = USER_CODE_BASE; // EIP: user entry point

    // context switch ret address
    *(--sp) = (uint32_t)usermode_trampoline;

    *(--sp) = 0; // EBP
    *(--sp) = 0; // EBX
    *(--sp) = 0; // ESI
    *(--sp) = 0; // EDI <-- kernel_esp points here

    proc->kernel_esp = (uint32_t)sp;

    // everything is set up, NOW it's safe for the scheduler to pick this up
    proc->state = PROCESS_READY;

    // restore interrupt state to whatever it was before
    __asm__ volatile("popfl" ::: "memory");

    kprintf("[UPROCESS] Created '%s' (PID %u) code = 0x%x stack=0x%x\n", proc->name, proc->pid, USER_CODE_BASE, USER_STACK_TOP);
    return proc;
}

int process_kill(uint32_t pid) {
    if (pid == 0) {
        kprintf("[PROCESS] Nice try, can't kill the kernel\n");
        return -1;
    }

    process_t* proc = process_get(pid);
    if (!proc) return -1;

    if (proc->state == PROCESS_ZOMBIE || proc->state == PROCESS_UNUSED) {
        return -1;
    }

    kprintf("[PROCESS] Killing '%s' (PID %u)\n", proc->name, proc->pid);
    proc->exit_code = -1;

    if (proc == current_process) {
        // killing ourselves. mark zombie and schedule away,
        // the scheduler will reap us later
        proc->state = PROCESS_ZOMBIE;
        schedule();
        while (1) { __asm__ volatile("hlt"); }
    }

    // not the current process, safe to free immediately
    proc->state = PROCESS_ZOMBIE;
    process_free(proc);
    return 0;
}

const char* process_state_name(process_state_t state) {
    switch (state) {
        case PROCESS_UNUSED:  return "UNUSED";
        case PROCESS_READY:   return "READY";
        case PROCESS_RUNNING: return "RUNNING";
        case PROCESS_BLOCKED: return "BLOCKED";
        case PROCESS_ZOMBIE:  return "ZOMBIE";
        default:              return "???";
    }
}

int process_spawn_test(const char* name) {
    for (uint32_t i = 0; i < NUM_TEST_PROGRAMS; i++) {
        if (name_equals(name, test_programs[i].name)) {
            process_t* proc = uprocess_create(
                test_programs[i].code,
                test_programs[i].size,
                test_programs[i].name
            );
            if (!proc) return -1;
            return (int)proc->pid;
        }
    }
    return -1;
}

const char* process_get_test_name(int index) {
    if (index < 0 || (uint32_t)index >= NUM_TEST_PROGRAMS) return NULL;
    return test_programs[index].name;
}


