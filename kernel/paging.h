#ifndef PAGING_H
#define PAGING_H

#include <stdint.h>

#define PAGE_SIZE 4096

// Higher-half kernel offset
// Virtual = Physical + KERNEL_VIRTUAL_BASE
// Physical = Virtual - KERNEL_VIRTUAL_BASE
#define KERNEL_VIRTUAL_BASE 0xC0000000
#define PHYS_TO_VIRT(addr) ((uint32_t)(addr) + KERNEL_VIRTUAL_BASE)
#define VIRT_TO_PHYS(addr) ((uint32_t)(addr) - KERNEL_VIRTUAL_BASE)

// Page directory/entry flags
#define PAGE_PRESENT 0x001 // Page is present in memory
#define PAGE_WRITE 0x002 // Page is writable (0 = read-only)
#define PAGE_USER 0x004 // Page is accessible from user mode
#define PAGE_ACCESSED 0x020 // CPU sets this when page is accessed
#define PAGE_DIRTY 0x040 // CPU sets this when page is written to

// # of entries in page dir and page tables
#define PAGE_ENTRIES 1024

// Page dir entry (points to a page table)
typedef uint32_t page_dir_entry_t;

// Page table entry (points to a 4KB physical page)
typedef uint32_t page_table_entry_t;

// Init paging with higher-half mapping for kernel
void paging_init(void);

// Map a virtual addr to a physical addr
void paging_map_page(uint32_t virtual_addr, uint32_t physical_addr, uint32_t flags);

// Unmap a virtual addr
void paging_unmap_page(uint32_t virtual_addr);

// Get physical addr for a virtual addr (returns 0 if not mapped)
uint32_t paging_get_physical(uint32_t virtual_addr);

// Flush TLB for a specific address
void paging_flush_tlb(uint32_t virtual_addr);

// get PHYSICAL address of the curr page dir (for CR3 / address space tracking)
uint32_t* paging_get_directory(void);

// create a brand new page dir, allocates a 4KB frame for the dir, clears it out,
// and copies over all the kernel-space entries. Returns PHYSICAL address.
uint32_t* paging_create_directory(void);

// switch the active page dir by loading a new PHYSICAL address into CR3
void paging_switch_directory(uint32_t* dir);

#endif
