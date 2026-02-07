/* Linker script for TupleOS kernel (higher-half)
 *
 * The kernel is loaded by GRUB at physical address 0x100000 (1MB),
 * but runs at virtual address 0xC0100000 (3GB + 1MB).
 *
 * The .boot section contains early startup code that runs before paging
 * is enabled — it lives at the physical load address. Everything else
 * (.text, .rodata, .data, .bss) has virtual addresses in the higher half,
 * but AT() directives keep their physical (load) addresses at 0x100000+.
 *
 * Memory layout after boot:
 *   Physical 0x00100000+ : kernel binary in RAM (where GRUB put it)
 *   Virtual  0xC0000000+ : how the kernel sees itself (via page tables)
 *   Virtual  0x00000000–0xBFFFFFFF : free for user processes (Phase 4)
 */

KERNEL_VIRTUAL_BASE = 0xC0000000;

ENTRY(_start)

/* Explicit program headers so the linker creates separate ELF segments.
 * Without this, the linker merges sections into segments unpredictably,
 * which can push the .boot data (containing the multiboot header) beyond
 * GRUB's 8KB scan window in the file. */
PHDRS
{
    boot PT_LOAD FLAGS(5);    /* R + X */
    text PT_LOAD FLAGS(5);    /* R + X */
    data PT_LOAD FLAGS(6);    /* R + W */
}

SECTIONS
{
    /* Boot code at physical 1MB — runs before paging is enabled.
     * This includes the multiboot header (must be in first 8KB of binary)
     * and the assembly that sets up temporary page tables. */
    . = 1M;

    .boot ALIGN(4K) :
    {
        *(.multiboot)
        *(.boot)
    } :boot

    /* Jump the virtual address counter into the higher half.
     * The physical (load) addresses continue sequentially after .boot
     * thanks to the AT() directives below. */
    . += KERNEL_VIRTUAL_BASE;

    .text ALIGN(4K) : AT(ADDR(.text) - KERNEL_VIRTUAL_BASE)
    {
        *(.text)
    } :text

    .rodata ALIGN(4K) : AT(ADDR(.rodata) - KERNEL_VIRTUAL_BASE)
    {
        *(.rodata)
    } :text

    .data ALIGN(4K) : AT(ADDR(.data) - KERNEL_VIRTUAL_BASE)
    {
        *(.data)
    } :data

    .bss ALIGN(4K) : AT(ADDR(.bss) - KERNEL_VIRTUAL_BASE)
    {
        *(COMMON)
        *(.bss)
    } :data

    /* _kernel_end is now a VIRTUAL address (0xC01xxxxx).
     * PMM must convert to physical before comparing with frame addresses. */
    _kernel_end = .;
}
