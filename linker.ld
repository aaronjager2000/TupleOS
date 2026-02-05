/* Linker script for TupleOS kernel
 * When you compile boot.asm and kernel.c, you get "object files" (.o files)
 * The linker combines these two into one final binary (the kernel).
 * But it needs to know what the entry point is, wheere in memory should each section be placed, and what order should sections appear in
 * On x86, after GRUB loads us into protected mode, physical memory looks like:
 * 0x00000000 - 0x000FFFFF -> First 1MB (reserved: BIOS, VGA memory at 0xb80000, etc.)
 * 0x00100000 - onwards -> Free memory (this is where kernels get loaded)
 *
 * Grub loads our kernel at the 1MB marks (0x100000) by convention, so we tell the linker: "arrange everything starting at address 0x100000"
 */

/* This tells the linker that execution begins at the symbol _start, which we defined in boot.asm */
ENTRY(_start)

SECTIONS
{
. = 1M;
.text ALIGN(4K) :
{
    *(.multiboot)
    *(.text)
}

.rodata ALIGN(4K) :
{
    *(.rodata)
}

.data ALIGN(4K) :
{
    *(.data)
}

.bss ALIGN(4K) :
{
    *(COMMON)
    *(.bss)
}
}