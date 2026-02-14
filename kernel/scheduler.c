#include "scheduler.h"
#include "process.h"
#include "vmm.h"
#include "kprintf.h"
#include "tss.h"

// asm function
extern void context_switch(uint32_t* old_esp_ptr, uint32_t new_esp);

// scheduling disabled until init, prevents the timer handler from trying to schedule before the process table and kernel threads are set
static int scheduler_enabled = 0;

void scheduler_init(void) {
    scheduler_enabled = 1;
    kprintf("[SCHEDULER] Round Robin scheduler initialized\n");
}

void schedule(void) {
    if (!scheduler_enabled) return;

    // reap any zombie processes that aren't the current one
    // this cleans up after sys_exit and process_kill without needing a dedicated reaper thread
    for (int i = 0; i < MAX_PROCESSES; i++) {
        process_t* zproc = process_get_by_slot(i);
        if (zproc && zproc->state == PROCESS_ZOMBIE && zproc != process_current()) {
            process_free(zproc);
        }
    }

    process_t* current = process_current();
    if (!current) return;

    int current_slot = -1;
    for (int i = 0; i < MAX_PROCESSES; i++) {
        if (process_get_by_slot(i) == current) {
            current_slot = i;
            break;
        }
    }
    if (current_slot == -1) return; // ideally shouldn't happen

    int next_slot = -1;
    for (int i = 0; i < MAX_PROCESSES; i++) {
        int slot = (current_slot + i) % MAX_PROCESSES;
        process_t* proc = process_get_by_slot(slot);
        if (proc && proc->state == PROCESS_READY) {
            next_slot = slot;
            break;
        }
    }

    // nothing else to run, curr process keeps the CPU
    if (next_slot == -1) return;

    process_t* next = process_get_by_slot(next_slot);

    // update process states, only demote current to READy if it's still RUNNING, (it might be a ZOMBIE or BLOCKED, in which case leave it alone)
    if (current->state == PROCESS_RUNNING) {
        current->state = PROCESS_READY;
    }
    next->state = PROCESS_RUNNING;

    process_set_current(next);

    if (next->page_directory != current->page_directory) {
        vmm_switch_address_space(next->address_space);
    }

    tss_set_kernel_stack(next->kernel_stack + KERNEL_STACK_SIZE);

    // THE ACTUAL CONTEXT SWITCH

    // This call saves our ESP into current->kernel_esp, then loads next->kernel_esp into ESP. When it returns, we're on a different stack entirely
    // From this process's persepctive, the call "blocks" until some future timer tick switches back to us

    context_switch(&current->kernel_esp, next->kernel_esp);
}

// wake up any sleeping processes whose timer expired
// called every tick from timer_handler, right before schedule()
// wake_tick > 0 distinguishes "sleeping" from "blocked on something else" (I/O etc later)
void scheduler_wake_sleepers(uint32_t current_ticks) {
    for (int i = 0; i < MAX_PROCESSES; i++) {
        process_t* proc = process_get_by_slot(i);
        if (proc && proc->state == PROCESS_BLOCKED && proc->wake_tick > 0) {
            if (current_ticks >= proc->wake_tick) {
                proc->wake_tick = 0;
                proc->state = PROCESS_READY;
            }
        }
    }
}

// For 64 processes, a linear scan on every tick is fast. At 100Hz, we scan 64 slots 100 times per second, that's 6400 comparisons/sec, trivial for any CPU
// A linked-list run queue would be O(1) for picking the next process, but adds complexity for insertion/removal and cache unfriendly pointer chasing, not worth it at this scale (we can look at implementing this later)
// The round robin algo also guarantees fairness, if there are N runnable processes, each gets 1/N of the CPU time, no process can starve another
// the downside is that every process gets equal time regardless of importance, that's what the priority field in the PCB is for later