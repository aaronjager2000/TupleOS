# boot.asm / This is the assembly entry point for the kernel
.set ALIGN, 1<<0 # = 1 in binary: 0001
.set MEMINFO, 1<<1 # = 2 in binary: 0010
.set FLAGS, ALIGN | MEMINFO # | is the "bitwise OR" operator, we essentially create a new binary 0011
.set MAGIC, 0x1BADB002
.set CHECKSUM, -(MAGIC + FLAGS)

# Declare a multiboot header that marks the program as a kernel. These are magic vals that are documented in the multiboot standard.
# The bootloader will search for this signature in the first 8 KiB of the kernel file, aligned at a 32-bit boundary.
# This signature is in its own section so the header can be forced to be within the first 8 KiB of the kernel file.
.section .multiboot
.align 4
.long MAGIC
.long FLAGS
.long CHECKSUM

# The multiboot standard does not define the value of the stack pointer register (esp) and it is set up to the kernel to provide a stack. This allocates room for a small stack by creating a symbol at the bottom fo it, then allocating 16384 bytes for it
# and finally creating a symbol at the top. The stack grows which means the kernel file is smaller because it does not contain an uninitialized stack.
# The stack on x86 must be 16-byte aligned according to the System V ABI standard and de-facto extensions. The compiler will assume the stack is properly aligned and failure to align the stack will result in UB.
.section .bss
.align 16
stack_bottom:
.skip 16384 # 16 KiB
stack_top:

# The linker script specifies _start as tthe entry point to the kernel and the bootloader will jump to this pos once the kernel has been loaded
# It doesn't make sense to reutrn from this function as the bootloader is gone.
.section .text
.global _start
.type _start, @function
_start:
# The bootloader has loaded us into 32-bit protected mode on a x86 machine. Interrupts are disabled, paging is disabled. The processor state is as defined in the multiboot standard. The kernel has full control of the CPU.
# The kernel can only make use of hardware features and any code it provides as part of itself. There's no printf function, unless the kernel provides its own <stdio.h> header and a printf implementation
# There are no security restrictions, nosafeguards, no debugging mechanisms, only what the kernel provides itself. It has absolute and complete power over the machine.
# To set up a stack, we set the esp register to point to the top of the stack (as it grows downards on x86 systems). This is necessarily done in assembly as languages such as C can't function without a stack.
mov $stack_top, %esp

# This is a good place to initialize crucial processor state before the high-level kernel is entered. It's best to minimize the early environment where crucial features are offline. Note that the processor is not fully initialized yet.
# Features such as floating point instructions and instruction set extensions are not initialized yet. The GDT should be loaded here. Paging should be enabled here.
# C++ features such as global constructors and exceptions will require runtime support to work as well

# Enter the high level kernel. The ABI requires the stack is 16-byte aligned at the time of the call instruction (which afterwards pushes the return pointer of size 4 bytes)
# The stack was originially 16-byte aligned above and we've pushed a multiple of 16 bytes to the stack since (pushed 0 bytes so far), so the alignment has thus been preserved and the call is well defined.
call kernel_main

# If the system has nothing more to do, put the computer into an infinite loop. To do that:
# 1: Disable interrupts with cli (clear interrupt enable in eflags). They are already disabled by the bootloader, so this is not needed. Mind that you might later enable interrupts and return from kernel_main (which is sort of nonsensical to do)
# 2: Wait for the next interrup to arrive with hlt (halt instruction). Since they are disabled, this will lock up the comptuer.
# 3: Jump to the hlt instruction if it ever wakes up due to a non-maskaple interrupt occuring or due to system management mode.
cli
1: hlt
jmp 1b

.size _start, . - _start
