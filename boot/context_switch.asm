# core multitasking file

# Two functions will live here
#   1. context_switch() swaps kernel stacks between two processes
#   2. kthread_trampoline is the entry point for brand new kernel threads

# We need to write this in assembly because C can't directly manipulate ESP
# The compiler manages the stack implicitly, if you changed ESP out from under it, the compiler's assumptions about where local vars and return addrs live would be wrong
# So we do the swap in assembly where we have full control

.section .text
.extern kthread_exit

# void context_switch(uint32_t* old_esp_ptr, uint32_t new_esp)

# saves the curr kernel stack pointer into *old_esp_ptr
# then loads new_esp as the curr stack pointer

# This is the ONLY function in the entire OS that changes ESP
# Everything else, scheduling policy, process state, address spaces, is just bookkeeping
# This is where tehe actual switch happens

# We only save callee-saved registers (EBX, ESI, EDI, EBP) because the C calling convetion guarantees the caller (schedule) already saved everything else it cares about
# EAX, ECX, EDX are caller-saved, so schedule() doesn't expect them to survive a function call

# Stack layout after saving (low addr at top)

# EDI <-- ESP (saved into old->kernel_esp)
# ESI
# EBX
# EBP
# return addr <-- schedule() will resume here when switched back

.global context_switch
context_switch:
    # Grab arguments BEFORE pushing (pushes change ESP, shifting offsets)
    mov 4(%esp), %eax # eax = &old_process->kernel_esp
    mov 8(%esp), %ecx # ecx = new_process->kernel_esp

    # Save callee-saved registers on the OLD (current) stack
    push %ebp
    push %ebx
    push %esi
    push %edi

    # save current ESP into old process's PCB
    mov %esp, (%eax) # *old_esp_ptr = ESP

    # THE SWITCH
    # After this instruction, we are on a completely different stack
    # Every local var, every return addr, everything is now from the NEW process's perspective
    mov %ecx, %esp # ESP = new_esp

    # Rstore callee-saved registers from the NEW stack
    pop %edi
    pop %esi
    pop %ebx
    pop %ebp

    # return to wherever the new process left off
    # for a process that was previously preempted: returns into schedule()
    # which returns into timer_handler, which returns through isr_common, which does popa + iret back to the process's code, complicated asf tbh
    # for a brand new thread it's much simpler: returns to kthread_trampoline (see below)
    ret

# kthread_trampoline, first code a new kernel thread executes

# when kthread_create() sets up a new thread's stack, it places this function's addr where context_switch's `ret` will find it
# it also puts the real entry function pointer in the EBX slot

# so when context_switch pops registers and rets to us:
#  EBX = the actual entry function pointer in the EBX slot
#  Interrupts are DISABLED (we came from schedule() in the timer ISR)

# We need to:
#  1. Enable interrupts (the thread should run with interrupts on)
#  2. Call the entry function
#  3. If it returns, clean up the thread

.global kthread_trampoline
kthread_trampoline:
    sti # enable interrupts, we're starting fresh, not in an ISR
    call *%ebx # call the entry function (pointer was in EBX)
    call kthread_exit # if entry() returns, clean up this thread
1:  hlt # safety net, kthread_ewxit should never return
    jmp 1b

# HOW CONTEXT_SWITCH WORKS, STEP BY STEP
# Let's say process A is running and the timer preempts it to switch to process B

# Before the switch: Process A's kernel stack:
# (high addr)
# ┌──────────────────────────┐
# │ interrupt frame          │  ← pushed by CPU + isr_common when timer fired
# │ (EIP, CS, EFLAGS, regs)  │
# ├──────────────────────────┤
# │ interrupt_handler frame  │  ← C function call frames
# │ timer_handler frame      │
# │ schedule() frame         │
# ├──────────────────────────┤
# │ return addr (to schedule)│  ← pushed by `call context_switch`
# ├──────────────────────────┤
# │ EBP                      │  ← context_switch pushes these
# │ EBX                      │
# │ ESI                      │
# │ EDI                      │  ← ESP saved here → A->kernel_esp
# └──────────────────────────┘
# (low addr)

# After mov %ecx, %esp, now on process B's kernel stack:

# (high addr)
# ┌──────────────────────────┐
# │ B's interrupt frame      │  ← from when B was preempted earlier
# ├──────────────────────────┤
# │ B's C call frames        │
# ├──────────────────────────┤
# │ return addr (to schedule)│
# ├──────────────────────────┤
# │ EBP                      │
# │ EBX                      │
# │ ESI                      │
# │ EDI                      │  ← ESP loaded from B->kernel_esp
# └──────────────────────────┘

# The pop instructions restore B's callee-saved registers. The ret pops B's return address, which points into schedule()
# schedule() returns to timer_handler(), which returns into interrupt_handler(), which returns to isr_common.
# then isr_common does pop ds, popa, add $8 %esp, iret, and we're back in process B's code with all its registers restored. B picks up exactly where it left off, oblivious that it was ever paused
