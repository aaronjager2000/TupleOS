# boot.asm — Higher-half kernel entry point
#
# GRUB loads us at physical 0x100000 and jumps here with:
#   EAX = multiboot magic (0x2BADB002)
#   EBX = pointer to multiboot info struct (physical address)
#
# Our job:
#   1. Set up a temporary page directory that maps the first 4MB at both
#      the identity address (0x00000000) and the higher-half (0xC0000000)
#   2. Enable paging
#   3. Jump to the higher-half virtual address
#   4. Set up the stack and call kernel_main
#
# The identity map is temporary — it keeps the CPU from faulting mid-transition
# (we're executing at physical ~0x100000 when we flip paging on, so that
# address range needs to stay valid). paging_init() later builds the real
# page tables and the identity map disappears when the new CR3 is loaded.

.set ALIGN, 1<<0
.set MEMINFO, 1<<1
.set FLAGS, ALIGN | MEMINFO
.set MAGIC, 0x1BADB002
.set CHECKSUM, -(MAGIC + FLAGS)

.set KERNEL_VIRTUAL_BASE, 0xC0000000
.set KERNEL_PD_INDEX, (KERNEL_VIRTUAL_BASE >> 22)   # = 768

# Multiboot header — must be in first 8KB of kernel binary
.section .multiboot, "a", @progbits
.align 4
.long MAGIC
.long FLAGS
.long CHECKSUM

# BSS — page tables and stack
# These symbols have VIRTUAL addresses (0xC0xxxxxx) because .bss
# is linked in the higher half. The boot code subtracts
# KERNEL_VIRTUAL_BASE to get their physical addresses.

.section .bss
.align 4096

# Temporary boot page directory (1024 entries × 4 bytes = 4KB)
boot_page_directory:
.skip 4096

# Temporary boot page table (maps first 4MB)
boot_page_table:
.skip 4096

.align 16
stack_bottom:
.skip 16384   # 16 KiB
.global stack_top
stack_top:

# Boot code — runs at PHYSICAL addresses before paging
.section .boot, "ax", @progbits
.global _start
.type _start, @function

_start:
    # Save multiboot parameters in registers we won't clobber
    movl %eax, %esi     # magic number
    movl %ebx, %edi     # multiboot info pointer (physical)

    # ---- Fill the boot page table: identity map first 4MB ----
    # 1024 entries × 4KB per page = 4MB coverage
    # Each entry: physical_address | PRESENT | WRITABLE
    movl $(boot_page_table - KERNEL_VIRTUAL_BASE), %ebx
    movl $0, %ecx        # physical address counter

.fill_table:
    movl %ecx, %eax
    orl $0x003, %eax     # Present + Read/Write
    movl %eax, (%ebx)

    addl $4096, %ecx     # next 4KB page
    addl $4, %ebx        # next table entry
    cmpl $(1024 * 4096), %ecx   # done when we've mapped 4MB
    jl .fill_table

    # ---- Set up the boot page directory ----
    # Clear it first (all entries not-present)
    movl $(boot_page_directory - KERNEL_VIRTUAL_BASE), %ebx
    movl $0, %ecx

.clear_pd:
    movl $0x00000002, (%ebx, %ecx, 4)   # R/W but not present
    incl %ecx
    cmpl $1024, %ecx
    jl .clear_pd

    # Install the page table in TWO page directory slots:
    movl $(boot_page_table - KERNEL_VIRTUAL_BASE), %eax
    orl $0x003, %eax     # Present + Read/Write

    # PD[0]:   identity map — virtual 0x00000000 maps to physical 0x00000000
    movl $(boot_page_directory - KERNEL_VIRTUAL_BASE), %ebx
    movl %eax, (%ebx)

    # PD[768]: higher-half — virtual 0xC0000000 maps to physical 0x00000000
    movl %eax, (KERNEL_PD_INDEX * 4)(%ebx)

    # ---- Enable paging ----
    # Load page directory physical address into CR3
    movl %ebx, %cr3

    # Set PG bit (bit 31) in CR0
    movl %cr0, %eax
    orl $0x80000000, %eax
    movl %eax, %cr0

    # Paging is ON. Both 0x001xxxxx and 0xC01xxxxx resolve to the same
    # physical memory. We're still executing in the identity-mapped range.

    # Jump to the higher-half virtual address — this is the moment we
    # transition from "running at physical addresses" to "running at
    # virtual addresses in the higher half"
    mov $_higher_half_start, %eax
    jmp *%eax

# Higher-half entry — runs at VIRTUAL addresses after paging
.section .text
.global _higher_half_start

_higher_half_start:
    # We're now executing at 0xC0xxxxxx.
    # The identity map (PD[0]) is still active but will go away when
    # paging_init() loads a new page directory. No need to remove it here.

    # Set up the kernel stack (stack_top is a virtual address in .bss)
    movl $stack_top, %esp

    # Push multiboot parameters for kernel_main(magic, mbi)
    # mbi is still a physical address — that's fine because the identity
    # map is still active and pmm_init will parse it before paging_init
    # replaces the page directory
    pushl %edi    # multiboot info pointer
    pushl %esi    # multiboot magic number

    call kernel_main

    # If kernel_main returns (it shouldn't), halt forever
    cli
1:  hlt
    jmp 1b
