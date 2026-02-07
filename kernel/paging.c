/*
* IMPORTANT!!!!!!
* The PHYS_TO_VIRT macro only works for physical addresses within the first 16MB (the range mapped by paging_init). 
* If the PMM ever returns a frame above 16MB and you try to write to it via PHYS_TO_VIRT, it'll page fault. 
* With 128MB of RAM and the PMM's linear scan, frames are allocated from low addresses first, so this won't be an issue in practice. 
* But if we later add more RAM support, bump page_tables[4] to page_tables[32] (128MB coverage) and adjust the loop in paging_init accordingly.
*/

#include "paging.h"
#include "pmm.h"
#include "kprintf.h"

// Page dir, must be 4KB aligned
// Contains 1024 entries, each pointing to a page table
static page_dir_entry_t page_directory[PAGE_ENTRIES] __attribute__((aligned(4096)));

// Page tables for first 16MB mapped at 0xC0000000 (4 tables x 4MB each)
// PD indices 768-771
static page_table_entry_t page_tables[4][PAGE_ENTRIES] __attribute__((aligned(4096)));

// Pointer to curr page dir (virtual addr for reading/writing)
static page_dir_entry_t* current_page_directory = NULL;

// Load page dir address into CR3 (must be PHYSICAL address)
static void paging_load_directory(page_dir_entry_t* dir_phys) {
    __asm__ volatile("mov %0, %%cr3" :: "r"(dir_phys));
}

void paging_flush_tlb(uint32_t virtual_addr) {
    __asm__ volatile("invlpg (%0)" :: "r"(virtual_addr) : "memory");
}

void paging_init(void) {
    kprintf("Paging: Initializing higher-half mapping...\n");

    // Clear the page directory
    for (int i = 0; i < PAGE_ENTRIES; i++) {
        page_directory[i] = 0x00000002; // R/W but not present
    }

    // Map first 16MB of physical memory at virtual 0xC0000000
    // PD index 768 = 0xC0000000 >> 22
    // PD index 769 = 0xC0400000 >> 22
    // PD index 770 = 0xC0800000 >> 22
    // PD index 771 = 0xC0C00000 >> 22
    for (int t = 0; t < 4; t++) {
        for (int i = 0; i < PAGE_ENTRIES; i++) {
            uint32_t phys_addr = (t * PAGE_ENTRIES + i) * PAGE_SIZE;
            page_tables[t][i] = phys_addr | PAGE_PRESENT | PAGE_WRITE;
        }
        // PD entries store PHYSICAL addresses of page tables
        page_directory[768 + t] = VIRT_TO_PHYS((uint32_t)&page_tables[t]) | PAGE_PRESENT | PAGE_WRITE;
    }

    // Store current page dir (virtual address for kernel access)
    current_page_directory = page_directory;

    // Load page dir into CR3 (needs PHYSICAL address)
    paging_load_directory((page_dir_entry_t*)VIRT_TO_PHYS((uint32_t)page_directory));

    // Paging was already enabled in boot.asm — we just replaced the page directory.
    // The boot identity map (PD[0]) is now gone. We're purely higher-half.

    kprintf("Paging: Loaded, first 16MB mapped at 0xC0000000\n");
}

void paging_map_page(uint32_t virtual_addr, uint32_t physical_addr, uint32_t flags) {
    uint32_t pd_index = virtual_addr >> 22;
    uint32_t pt_index = (virtual_addr >> 12) & 0x3FF;

    if (!(page_directory[pd_index] & PAGE_PRESENT)) {
        // Allocate a new page table (PMM returns physical address)
        uint32_t new_table_phys = (uint32_t)pmm_alloc_frame();
        if (!new_table_phys) {
            kprintf("Paging: Failed to allocate page table\n");
            return;
        }

        // Clear the new page table (convert to virtual to write to it)
        page_table_entry_t* new_table = (page_table_entry_t*)PHYS_TO_VIRT(new_table_phys);
        for (int i = 0; i < PAGE_ENTRIES; i++) {
            new_table[i] = 0;
        }

        // Add to page dir (stores physical address, as hardware requires)
        page_directory[pd_index] = new_table_phys | PAGE_PRESENT | PAGE_WRITE | (flags & PAGE_USER);
    }

    // Get the page table: PD entry has physical addr, convert to virtual to dereference
    uint32_t table_phys = page_directory[pd_index] & 0xFFFFF000;
    page_table_entry_t* table = (page_table_entry_t*)PHYS_TO_VIRT(table_phys);

    // Map the page
    table[pt_index] = (physical_addr & 0xFFFFF000) | (flags & 0xFFF) | PAGE_PRESENT;

    paging_flush_tlb(virtual_addr);
}

void paging_unmap_page(uint32_t virtual_addr) {
    uint32_t pd_index = virtual_addr >> 22;
    uint32_t pt_index = (virtual_addr >> 12) & 0x3FF;

    if (!(page_directory[pd_index] & PAGE_PRESENT)) {
        return;
    }

    uint32_t table_phys = page_directory[pd_index] & 0xFFFFF000;
    page_table_entry_t* table = (page_table_entry_t*)PHYS_TO_VIRT(table_phys);
    table[pt_index] = 0;

    paging_flush_tlb(virtual_addr);
}

uint32_t paging_get_physical(uint32_t virtual_addr) {
    uint32_t pd_index = virtual_addr >> 22;
    uint32_t pt_index = (virtual_addr >> 12) & 0x3FF;
    uint32_t offset = virtual_addr & 0xFFF;

    if (!(page_directory[pd_index] & PAGE_PRESENT)) {
        return 0;
    }

    uint32_t table_phys = page_directory[pd_index] & 0xFFFFF000;
    page_table_entry_t* table = (page_table_entry_t*)PHYS_TO_VIRT(table_phys);

    if (!(table[pt_index] & PAGE_PRESENT)) {
        return 0;
    }

    return (table[pt_index] & 0xFFFFF000) + offset;
}

uint32_t* paging_get_directory(void) {
    // Return PHYSICAL address (for CR3 and address space tracking)
    return (uint32_t*)VIRT_TO_PHYS((uint32_t)page_directory);
}

uint32_t* paging_create_directory(void) {
    // Allocate a physical frame for the new page dir
    uint32_t dir_phys = (uint32_t)pmm_alloc_frame();
    if (!dir_phys) {
        kprintf("Paging: failed to allocate new page directory\n");
        return NULL;
    }

    // Convert to virtual to write to it
    uint32_t* new_dir = (uint32_t*)PHYS_TO_VIRT(dir_phys);

    // Start with everything not present
    for (int i = 0; i < PAGE_ENTRIES; i++) {
        new_dir[i] = 0x00000002;
    }

    // Copy ALL kernel-space PD entries (indices 768-1023)
    // This is what makes the kernel accessible from every address space
    for (int i = 768; i < 1024; i++) {
        new_dir[i] = page_directory[i];
    }

    // Return PHYSICAL address (for storing in address space and loading into CR3)
    return (uint32_t*)dir_phys;
}

void paging_switch_directory(uint32_t* dir_phys) {
    // Store virtual address for kernel access
    current_page_directory = (page_dir_entry_t*)PHYS_TO_VIRT((uint32_t)dir_phys);
    // Load physical address into CR3
    paging_load_directory((page_dir_entry_t*)dir_phys);
}
