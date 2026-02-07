#ifndef VMM_H
#define VMM_H

#include <stdint.h>
#include <stddef.h>

/*
* Virtual Memory Manager
* So paging.c gives us the raw tools to map/unmap individual pages, but it doesn't actually track what we've mapped or why.
* It's like having a filing cabinet with no labels on the drawers.
* 
* The VMM fixes that. It tracks "regions" of virtual memory (contiguous ranges with the same permissions and purpose)
* and groups them into "address spaces" (one page directory + all its regions)

* Right now we only have one address space (the kernel's), but when we get to Phase 4 and start running user processes,
* each process will get its own address space with its own page directory. The VMM is what makes that switch seamless

* The other big thing is the page fault handler. Without it, any access to an unmapped page causes a triple fault and QEMU just
* reboots with no explanation. The VMM hooks ISR 14 and gives us actual error messages, plus it sets us up for demand paging later
* (where we don't bother allocating physical frames until someone actually touches the page).
*/

// These describe what a region is being used for, mostly for bokkeeping and debugging right now
// But later on the page fault handler will use these to decvide how to handle faults
// Ex: a fault in a REGION_STACK might trigger automatic stack growth
typedef enum {
    REGION_FREE = 0, // not in use, available for mapping
    REGION_KERNEL_CODE, // kernel .text and .rodata (the code itself)
    REGION_KERNEL_DATA, // kernel .data and .bss (globals, statics)
    REGION_KERNEL_HEAP, // kmalloc territory (managed by kheap naturally)
    REGION_KERNEL_STACK, // kernel stack (the one set up in boot.asm)
    REGION_IDENTITY_MAP, // the first 16 MB identity map we set up in paging_init
    REGION_USER_CODE, // future: user process copde
    REGION_USER_DATA, // future: user process data
    REGION_USER_HEAP, // future: user process heap (brk/sbrk)
    REGION_USER_STACK, // future: user process stack
    REGION_MMIO, // memory-mapped I/O 
} vmm_region_type_t;

// permission flags for regions, these map pretty directly to the page table entry flags, but we track them separately
// because a region might be "logically writable" even if the page isn't currently present in memory
#define VMM_READ 0x01 // region can be read (basically always set)
#define VMM_WRITE 0x02 // region can be written to
#define VMM_EXEC 0x04 // region contains executable code (no NX bit on i686 though)
#define VMM_USER 0x08 // accessible from ring 3 (user mode)

// a virtual memory region. this tracks a contiguous range of virtual addresses that share the same perms and purpose
// similar concept to linux's vm_area_struct but way simpler cause we don't need to handle shared libs, mmap, copy-on-write or any of that stuff yet
typedef struct vmm_region {
    uint32_t base; // starting virtual address (page aligned)
    uint32_t size; // size in bytes (multiple of PAGE_SIZE)
    uint32_t flags; // VMM_READ | VMM_WRITE etc.
    vmm_region_type_t type; // what this region is for
    struct vmm_region* next; // linked list, sorted by base address
} vmm_region_t;

// how many regions we can track at once, using a static pool instead of kmalloc because we might need to create regions before the heap is ready
// and also because the heap itself needs to be tracked as a region (chicken and egg problem)
#define VMM_MAX_REGIONS 256

// an address space. wraps a page dir and all the regions mapped to it. right now there's just one (the kernel's) but each process wil get its own later
// the kernel regions get copies into every new address space so the kernel is always accessible regardless of which process is running
typedef struct {
    uint32_t* page_directory; // physical addr of the page dir
    vmm_region_t* regions; // linked list of memory regions
    uint32_t region_count; // track
} vmm_address_space_t;

void vmm_init(void);

// Will return NULL if we are out of memory
vmm_address_space_t* vmm_create_address_space(void);

// NEVER CALL THIS ON THE KERNEL ADDRESS SPACE
void vmm_destroy_address_space(vmm_address_space_t* space);

// We can swap CR3 to point at a different address space's page dir
// this is the context switch for memory, when the scheduler picks a new process, it calls this to flip to that process's view of memory
void vmm_switch_address_space(vmm_address_space_t* space);

// grabs pointer to kernel's address space
vmm_address_space_t* vmm_get_kernel_space(void);


// allocates physical frames and maps them into the given address space. the virtual address and size both need to be page-aligned
// returns 0 on sucess, -1 on failure
int vmm_map_region(vmm_address_space_t* space, uint32_t vaddr, uint32_t size, uint32_t flags, vmm_region_type_t type);

// only unmaps exact region matches (can't unmap half a region)
int vmm_unmap_region(vmm_address_space_t* space, uint32_t vaddr);

// search for a contiguous chunk of free virtual address space, useful when you need to map something but don't care where it goes
uint32_t vmm_find_free_region(vmm_address_space_t* space, uint32_t size, uint32_t start_hint);

vmm_region_t* vmm_find_region(vmm_address_space_t* space, uint32_t vaddr);

int vmm_is_mapped(uint32_t vaddr);

// dump all regions in an address space to serial console, purely for debugging
void vmm_dump_regions(vmm_address_space_t* space);


#endif