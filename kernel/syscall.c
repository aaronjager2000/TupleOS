#include "syscall.h"
#include "idt.h"
#include "process.h"
#include "scheduler.h"
#include "terminal.h"
#include "kprintf.h"
#include "paging.h"
#include "vmm.h"
#include "timer.h"

// syscall dispatch and implementation
//
// this is the ONLY door between user land and the kernel
// user code puts syscall number in EAX, args in EBX through EDI, does INT 0x80
// we figure out what they want, do it, stuff the result back in EAX, and iret back
//
// the full path: user INT 0x80 -> isr128 -> isr_common -> interrupt_handler
//   -> syscall_handler -> sys_whatever -> modify frame->eax -> return
//   -> isr_common popa + iret -> user code resumes with result in EAX

// forward declarations
static int32_t sys_exit(struct interrupt_frame* frame);
static int32_t sys_write(struct interrupt_frame* frame);
static int32_t sys_getpid(struct interrupt_frame* frame);
static int32_t sys_yield(struct interrupt_frame* frame);
static int32_t sys_sleep(struct interrupt_frame* frame);

// dispatch table: syscall number -> handler function
// NULL entries = not implemented yet
typedef int32_t (*syscall_fn_t)(struct interrupt_frame* frame);

static syscall_fn_t syscall_table[NUM_SYSCALLS] = {
    [SYS_EXIT]   = sys_exit,
    [SYS_WRITE]  = sys_write,
    [SYS_GETPID] = sys_getpid,
    [SYS_YIELD]  = sys_yield,
    [SYS_SLEEP]  = sys_sleep,
    // [5..7] = NULL (stretch goals)
};

// validate that a user space pointer is actually safe to dereference
// without this a malicious process could pass a kernel addr as a buffer
// and we'd happily read kernel memory and spit it out to the terminal
// thats a security hole you could drive a truck through
static int validate_user_buffer(const void* ptr, uint32_t len) {
    if (len == 0) return 1; // empty buffer is fine

    uint32_t start = (uint32_t)ptr;
    uint32_t end = start + len - 1;

    // overflow = someone passed a huge len trying to wrap around the addr space
    if (end < start) return 0;

    // must be entirely below kernel space
    if (end >= KERNEL_VIRTUAL_BASE) return 0;

    // make sure the memory is actually mapped in this process's addr space
    process_t* proc = process_current();
    if (!proc || !proc->address_space) return 0;

    vmm_region_t* start_region = vmm_find_region(proc->address_space, start);
    if (!start_region || !(start_region->flags & VMM_USER)) return 0;

    vmm_region_t* end_region = vmm_find_region(proc->address_space, end);
    if (!end_region || !(end_region->flags & VMM_USER)) return 0;

    return 1;
}

// the main dispatcher, registered as handler for INT 0x80
// reads syscall number from EAX, dispatches, writes return value back to EAX
// when isr_common does popa it restores our modified EAX so the user gets the result
static void syscall_handler(struct interrupt_frame* frame) {
    uint32_t syscall_num = frame->eax;

    if (syscall_num >= NUM_SYSCALLS || !syscall_table[syscall_num]) {
        kprintf("[SYSCALL] Unknown syscall %u from PID %u\n",
                syscall_num, process_current()->pid);
        frame->eax = (uint32_t)(-1);
        return;
    }

    int32_t result = syscall_table[syscall_num](frame);
    frame->eax = (uint32_t)result;
}

// ============================================================
// individual syscall implementations
// ============================================================

// sys_exit(int exit_code)
// EBX = exit code
// kills the calling process. marks it ZOMBIE, yields to scheduler
// we never come back cause the scheduler only picks READY processes
static int32_t sys_exit(struct interrupt_frame* frame) {
    int32_t code = (int32_t)frame->ebx;
    process_t* proc = process_current();

    kprintf("[SYSCALL] exit(%d) from '%s' (PID %u)\n", code, proc->name, proc->pid);

    proc->exit_code = code;
    proc->state = PROCESS_ZOMBIE;

    // switch to another process. we're ZOMBIE so scheduler will never switch back
    schedule();

    // should never get here but just in case
    while (1) { __asm__ volatile("hlt"); }
    return 0;
}

// sys_write(int fd, const char* buf, size_t len)
// EBX = fd (only 1 = stdout for now), ECX = buffer ptr, EDX = length
// validates the user pointer before touching it cause we're not animals
// returns bytes written or -1 on error
static int32_t sys_write(struct interrupt_frame* frame) {
    int fd = (int)frame->ebx;
    const char* buf = (const char*)frame->ecx;
    uint32_t len = frame->edx;

    if (fd != 1) return -1;

    // dont let someone write 4GB to the terminal
    if (len > 4096) len = 4096;

    // SECURITY: validate the user pointer before touching it
    if (!validate_user_buffer(buf, len)) {
        kprintf("[SYSCALL] write: bad pointer 0x%x (len=%u) from PID %u\n",
                (uint32_t)buf, len, process_current()->pid);
        return -1;
    }

    // we're still in the user's addr space (same CR3) so user pages are accessible
    for (uint32_t i = 0; i < len; i++) {
        terminal_putchar(buf[i]);
    }

    return (int32_t)len;
}

// sys_getpid()
// no args, just returns the PID. simplest syscall possible
static int32_t sys_getpid(struct interrupt_frame* frame) {
    (void)frame;
    return (int32_t)process_current()->pid;
}

// sys_yield()
// voluntarily gives up the CPU. process stays READY so it gets picked up again
// basically saying "i got nothing to do rn, let someone else run"
static int32_t sys_yield(struct interrupt_frame* frame) {
    (void)frame;
    // just call schedule(). it sees our state is RUNNING, demotes us to READY,
    // picks the next READY process, and switches. when we get switched back
    // schedule() returns here and we return 0
    schedule();
    return 0;
}

// sys_sleep(uint32_t ms)
// EBX = milliseconds to sleep
// blocks the process, sets a wake timer, lets someone else run
// timer is 100Hz so granularity is 10ms. sleep(1) actually sleeps ~10ms
// not precise but good enough for an OS that doesnt even have a filesystem yet
static int32_t sys_sleep(struct interrupt_frame* frame) {
    uint32_t ms = frame->ebx;

    if (ms == 0) return 0; // sleep(0) is a noop

    process_t* proc = process_current();

    // round up so sleep(1) sleeps for at least one full tick
    // instead of potentially waking immediately
    uint32_t ticks_to_sleep = (ms + 9) / 10;

    proc->wake_tick = timer_get_ticks() + ticks_to_sleep;
    proc->state = PROCESS_BLOCKED;

    // switch to another process. the timer handler will set us
    // back to READY when ticks >= wake_tick
    schedule();

    // we're back, someone woke us up
    proc->wake_tick = 0;
    return 0;
}

void syscall_init(void) {
    idt_register_handler(128, syscall_handler);
    kprintf("[SYSCALL] System call interface initialized (INT 0x80)\n");
}
