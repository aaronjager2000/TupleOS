#ifndef SYSCALL_H
#define SYSCALL_H

#include <stdint.h>

// System Call Interface
//
// the ONLY way for user mode (ring 3) code to ask the kernel for anything
// user puts syscall number in EAX, args in EBX-EDI, does INT 0x80
// we dispatch to the right handler, do the work, put the result in EAX, iret back
//
// ABI (matches Linux i386 cause why reinvent the wheel):
//   EAX = syscall number
//   EBX = arg1
//   ECX = arg2
//   EDX = arg3
//   ESI = arg4
//   EDI = arg5
//   return value in EAX

// syscall numbers
#define SYS_EXIT    0   // terminate current process
#define SYS_WRITE   1   // write to terminal (fd=1)
#define SYS_GETPID  2   // get current PID
#define SYS_YIELD   3   // voluntarily give up CPU
#define SYS_SLEEP   4   // sleep for N milliseconds

// stretch goals for later:
// #define SYS_FORK    5
// #define SYS_EXEC    6
// #define SYS_WAIT    7

#define NUM_SYSCALLS 8  // dispatch table size (room for the stretch goals)

// register INT 0x80 handler, call once during boot
void syscall_init(void);

#endif
