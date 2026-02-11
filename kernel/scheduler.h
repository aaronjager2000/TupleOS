#ifndef SCHEDULER_H
#define SCHEDULER_H

#include "process.h"

// Round robin scheduler

// The simplest possible scheduling policy: every process gets one timer tick (10ms at 100Hz) before being preempted.
// We can the process table circularly, picking the next READY process

// No priorities or time quanta/run queues, just a fair even rotation
// This is enough to demonstrate preemptive multitasking. We can add priority levels and var time slices later without changing the context switch mechanism

void scheduler_init(void);

// If no other process is runnable, returns immediately (current process will continue to run)
void schedule(void);



#endif