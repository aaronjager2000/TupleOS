#include "vmm.h"
#include "pmm.h"
#include "paging.h"
#include "kheap.h"
#include "kprintf.h"
#include "idt.h"
#include "serial.h"

static vmm_address_space_t kernel_space;
static vmm_address_space_t* current_space = NULL;


// we could use kmalloc for these region structs but there's a chicken and egg issue
// the heap itself is a region, so we need regions before we have a heap
// 256 is plenty for now, even a complex process wont have more than maybe 20-30 regions (code, data, heap, stack, shared libs, mmap areas)
static vmm_region_t region_pool[VMM_MAX_REGIONS];
static uint32_t region_pool_used = 0;

static vmm_region_t* alloc_region(void) {
    if (region_pool_used >= VMM_MAX_REGIONS) {
        // this would be pretty catastrophic but shouldn't happen rn with 256 slots
        kprintf("VMM: ran out of region slots, this is bad\n");
        return NULL;
    }
    vmm_region_t* r = &region_pool[region_pool_used++];
    r->base = 0;
    r->size = 0;
    r->flags = 0;
    r->type = REGION_FREE;
    r->next = NULL;
    return r;
}

// keep this list sorted by base address so we can do gap-finding efficiently 
// just walk the list and look for gaps between neighbors
static void insert_region(vmm_address_space_t* space, vmm_region_t* region) {
    if (!space->regions || region->base < space->regions->base) {
        region->next = space->regions;
        space->regions = region;
        space->region_count++;
        return;
    }

    // walk the list to find where to insert
    vmm_region_t* curr = space->regions;
    while (curr->next && curr->next->base < region->base) {
        curr = curr->next;
    }
    region->next = curr->next;
    curr->next = region;
    space->region_count++;
}

static void remove_region(vmm_address_space_t* space, vmm_region_t* region) {
    if (space->regions == region) {
        space->regions = region->next;
        space->region_count--;
        return;
    }

    vmm_region_t* curr = space->regions;
    while (curr->next && curr->next != region) {
        curr = curr->next;
    }
    if (curr->next == region) {
        curr->next = region->next;
        space->region_count--;
    }
}


// add a region to an address space without actuall touching page tables
// this is for registering regions that are already mapped (like the identity map that paging_init set up before the VMM even existed)
// we're basically just telling the VMM "hey, this range is spoken for"
static vmm_region_t* register_existing_region(vmm_address_space_t* space, uint32_t base, uint32_t size, uint32_t flags, vmm_region_type_t type) {
    vmm_region_t* region = alloc_region();
    if (!region) return NULL;

    region->base = base;
    region->size = size;
    region->flags = flags;
    region->type = type;

    insert_region(space, region);
    return region;
}

// ISR 14 PAGE FAULT HANDLER
// This is where the magic happens, or where shit blows up :(, when the cpu tries to access a virtual addr, that's either not mapped or has the wrong perms
// it triggers interrupt 14 and puts the faulting address in the CR2 register

// The error code the CPU pushes tells us what went wrong
// bit 0: 0 = page not present, 1 = protection violation
// bit 1: 0 = read access, 1 = write access
// bit 2: 0 = kernel mode, 1 = user mode

// right now we mostly just print helpful debug info and halt. But the structure is here for demand paging:
// if a region exists but the page isn't present, we could allocate a frame on the fly instead of panicking

static void page_fault_handler(struct interrupt_frame* frame) {
    // CR2 holds the virtual addr that caused the fault, the CPU loads this automatically, we just need to read it
    uint32_t faulting_addr;
    __asm__ volatile("mov %%cr2, %0" : "=r"(faulting_addr));

    int present = frame->error_code & 0x1; // was the page present?
    int write = frame->error_code & 0x2; // was it a write?
    int user = frame->error_code & 0x4; // were we in user mode?

    // print something useful so we can actually debug this, without this handler, a page fault just triple faults the CPU and QEMU reboots with zero indication of what went wrong
    // ask me how many hours I wasted before adding this...
    kprintf("\n!!!!!!! PAGE FAULT !!!!!!!!!!");
    kprintf("Address: 0x%x\n", faulting_addr);
    kprintf("Error: %s %s in %s mode\n", present ? "protection violation" : "page not present", write ? "(write)" : "(read)", user ? "user" : "kernel");
    kprintf("At EIP: 0x%x\n", frame->eip);

    // also dump to serial in case the VGA output is messed up
    // which it might be if the fault happened in the display code
    serial_printf("PAGE FAULT: addr=0x%x err=0x%x eip=0x%x\n", faulting_addr, frame->error_code, frame->eip);

    // check if the faulting addr falls inside a known region, if it does the access is "logically valid", the memory was supposed to be there we just havent set up the page yet
    // this is the hook point for demand paging later
    if (current_space) {
        vmm_region_t* region = vmm_find_region(current_space, faulting_addr);

        if (region && !present) {
            /*
             * the address is in a valid region but the page isn't mapped yet.
             * this is where demand paging would go, allocate a physical frame,
             * map it, and return from the fault like nothing happened.
             *
             * for now though, we're not doing demand paging yet, so if we hit
             * this it means something didn't get mapped that should have been.
             * still useful to know it's a known region vs total garbage */
             kprintf(" -> Address is in region: type=%u, base=0x%x, size=0x%x\n", region->type, region->base, region->size);
             kprintf(" -> This region exists but page isn't present\n");
             kprintf(" -> (demand paging not implemented yet, halting)\n");
        } else if (region && present) {
            // the page IS present but we still faulted, which means it's a permission violation.
            // tried to write to a RO page, or user mode tried to touch a supervisor page
            kprintf(" -> Protection violation in region type-%u\n", region->type);
            if (write && !(region->flags & VMM_WRITE)) {
                kprintf(" -> Write to read-only region\n");
            }
            if (user && !(region->flags & VMM_USER)) {
                kprintf(" -> User access to kernel region\n");
            }
        } else {
            // address isn't in any tracked region. this is a legit segfault
            // the program (or kernel) accessed memory it had no business touching, in a real OS with user processes, this is where you'd send SIGSEGV
            kprintf(" -> Address not in any mapped region (segfault)\n");
        }
    }

    // for now, we just halt. can't really recover form a kernel page fault in any meaningful way yet, once we have user processes, we'll kill the faulting process instead of halting the whole system
    kprintf("System halted.\n");
    __asm__ volatile("cli; hlt");
}

static uint32_t vmm_flags_to_page_flags(uint32_t vmm_flags) {
    uint32_t pf = PAGE_PRESENT;
    if (vmm_flags & VMM_WRITE) pf |= PAGE_WRITE;
    if (vmm_flags & VMM_USER) pf |= PAGE_USER;
    return pf;
}

void vmm_init(void) {
    kprintf("VMM: Initializing...\n");

    // paging_init already created the page dir so we just need to set up the kernel address space
    kernel_space.page_directory = paging_get_directory();
    kernel_space.regions = NULL;
    kernel_space.region_count = 0;
    current_space = &kernel_space;

    // register the regions that already exist, paging_init identity-mapped the first 16MB, and kheap_init carved out a heap starting at 0x400000
    // we need to tell the VMM about these so it doesn't try to map something on top of them

    // first 4KB is intentionally left unmapped as a null guard page, any null pointer dereference will fault immediately instead of silently reading/writing address 0

    // First 1MB of physical memory mapped at 0xC0000000+ (VGA at 0xC00B8000, etc.)
    register_existing_region(&kernel_space, 0xC0001000, 0x000FF000, VMM_READ | VMM_WRITE, REGION_MMIO);

    // kernel code and data lives from 1MB up to wherever _kernel_end is
    // the linker script putsw it all starting at 0x100000
    extern uint32_t _kernel_end;
    uint32_t kend = ((uint32_t)&_kernel_end + PAGE_SIZE - 1) & ~(PAGE_SIZE -1);
    // _kernel_end is already a virtual address (0xC01xxxxx)
    register_existing_region(&kernel_space, 0xC0100000, kend - 0xC0100000, VMM_READ | VMM_WRITE | VMM_EXEC, REGION_KERNEL_CODE);


    // the rest of the identity mapped area between kernel end and where the heap starts
    // we cap it at 0x400000 (heap start) instead of 0x1000000 (16MB) because the heap
    // takes over from 0x400000 onward and we don't want overlapping regions
    if (kend < 0xC0400000) {
        register_existing_region(&kernel_space, kend, 0xC0400000 - kend, VMM_READ | VMM_WRITE, REGION_IDENTITY_MAP);
    }

    // the kernel heap at 0x400000, the kheap manages its own expansion, but we want the VMM to know this range is spoken for so the vmm_find_free_region won't hand out addresses in the middle of the heap
    register_existing_region(&kernel_space, 0xC0400000, 16 * 1024 * 1024, VMM_READ | VMM_WRITE, REGION_KERNEL_HEAP);

    // install the page fault handler. ISR 14 is the page fault exception. without this, any page fault causes a double fault which causes a triple fault which reboots the machine
    // not exactly helpful for debugging
    idt_register_handler(14, page_fault_handler);

    kprintf("VMM: Initialized with %u regions\n", kernel_space.region_count);
    vmm_dump_regions(&kernel_space);
}

vmm_address_space_t* vmm_create_address_space(void) {
    // allocate address space struct from the heap
    vmm_address_space_t* space = (vmm_address_space_t*)kmalloc(sizeof(vmm_address_space_t));
    if (!space) {
        kprintf("VMM: couldn't allocate address space struct\n");
        return NULL;
    }

    // create a fresh page dir
    // paging_create_directory gives us a new 4KB aligned page dir with the kernel mappings already copied in, so the kernel is accessible from this new address space too
    space->page_directory = paging_create_directory();
    if (!space->page_directory) {
        kprintf("VMM: couldn't create page directory\n");
        kfree(space);
        return NULL;
    }

    space->regions = NULL;
    space->region_count = 0;

    // copy the kernel regions into the new address space's region list, the page tables are already shared but we need the VMM's bookkeeping to match
    vmm_region_t* kr = kernel_space.regions;
    while (kr) {
        register_existing_region(space, kr->base, kr->size, kr->flags, kr->type);
        kr = kr->next;
    }
    return space;
}

void vmm_destroy_address_space(vmm_address_space_t* space) {
    if (!space || space == &kernel_space) {
        // dont destroy the kernel's address space lol
        kprintf("VMM: refusing to destroy kernel address space\n");
        return;
    }

    // walk the line for all regions and free the ones that aren't kernel regions
    vmm_region_t* region = space->regions;
    while (region) {
        vmm_region_t* next = region->next;

        if (region->type >= REGION_USER_CODE) {
            uint32_t addr = region->base;
            uint32_t end = region->base + region->size;
            while (addr < end) {
                uint32_t phys = paging_get_physical(addr);
                if (phys) {
                    paging_unmap_page(addr);
                    pmm_free_frame((void*)phys);
                }
                addr += PAGE_SIZE;
            }
        }

        region = next;
    }

    // free the page dir itself
    pmm_free_frame(space->page_directory);
    kfree(space);
}

void vmm_switch_address_space(vmm_address_space_t* space) {
    if (!space || !space->page_directory) return;

    current_space = space;

    // this is the actual context switch for memroy, loading a new value into CR3 flushes the entire TLB, so the CPU starts translating virtual addresses through the new page dir
    // from this point on, the virtual->physical mapping is completely different (except for kernel pages which are the same everywhere)
    paging_switch_directory(space->page_directory);
}

vmm_address_space_t* vmm_get_current_space(void) {
    return current_space;
}

vmm_address_space_t* vmm_get_kernel_space(void) {
    return &kernel_space;
}

int vmm_map_region(vmm_address_space_t* space, uint32_t vaddr, uint32_t size, uint32_t flags, vmm_region_type_t type) {
    if (!space) return -1;
    if (vaddr & (PAGE_SIZE - 1)) return -1; // must be page-aligned
    if (size & (PAGE_SIZE - 1)) return -1;

    // make sure this range doesn't overlap with an existing region, we walk the whole list and check
    // not the fastest approach but with <256 regions it doesn't matter tbh
    vmm_region_t* existing = space->regions;
    while (existing) {
        uint32_t existing_end = existing->base + existing->size;
        uint32_t new_end = vaddr + size;

        if (vaddr < existing_end && new_end > existing->base) {
            kprintf("VMM: region overlap! 0x%x-0x%x conflicts with 0x%x-0x%x\n", vaddr, new_end, existing->base, existing_end);
            return -1;
        }
        existing = existing->next;
    }

    uint32_t page_flags = vmm_flags_to_page_flags(flags);
    for (uint32_t offset = 0; offset < size; offset += PAGE_SIZE) {
        void* frame = pmm_alloc_frame();
        if (!frame) {
            // out of physical memory, undo what we already mapped.
            // this isn't the prettiest rollback but it works
            kprintf("VMM: out of physical memory during map\n");
            for (uint32_t undo = 0; undo < offset; undo += PAGE_SIZE) {
                uint32_t phys = paging_get_physical(vaddr + undo);
                paging_unmap_page(vaddr + undo);
                if (phys) pmm_free_frame((void*)(phys & 0xFFFFF000));
            }
            return -1;
        }

        paging_map_page(vaddr + offset, (uint32_t)frame, page_flags);

        // zero out the page. this is important for security (don't leak data from previous allocations) and for sanity (bss excepts zeroes)
        uint8_t* page_ptr = (uint8_t*)(vaddr + offset);
        for (uint32_t i = 0; i < PAGE_SIZE; i++) {
            page_ptr[i] = 0;
        }
    }

    // create the region tracking entry
    vmm_region_t* region = alloc_region();
    if (!region) {
        for (uint32_t offset = 0; offset < size; offset += PAGE_SIZE) {
            uint32_t phys = paging_get_physical(vaddr + offset);
            paging_unmap_page(vaddr + offset);
            if (phys) pmm_free_frame((void*)(phys & 0xFFFFF000));
        }
        return -1;
    }

    region->base = vaddr;
    region->size = size;
    region->flags = flags;
    region->type = type;
    insert_region(space, region);

    return 0;
}

int vmm_unmap_region(vmm_address_space_t* space, uint32_t vaddr) {
    if (!space) return -1;

    // find the region that starts at this exact address
    vmm_region_t* region = space->regions;
    while (region) {
        if (region->base == vaddr) break;
        region = region->next;
    }

    if (!region) {
        kprintf("VMM: no region at 0x%x to unmap\n", vaddr);
        return -1;
    }

    // don't let anyone unmap kernel regions through this API
    if (region->type <= REGION_IDENTITY_MAP) {
        kprintf("VMM: refusing to unmap kernel region at 0x%x\n", vaddr);
        return -1;
    }

    // unmap each page and free the physical frame behind it
    for (uint32_t offset = 0; offset < region->size; offset += PAGE_SIZE) {
        uint32_t addr = region->base + offset;
        uint32_t phys = paging_get_physical(addr);
        if (phys) {
            paging_unmap_page(addr);
            pmm_free_frame((void*)(phys & 0xFFFFF000));
        }
    }

    remove_region(space, region);
    return 0;
}

uint32_t vmm_find_free_region(vmm_address_space_t* space, uint32_t size, uint32_t start_hint) {
    if (!space) return 0;

    // align everything to page boundaries
    size = (size + PAGE_SIZE -1) & ~(PAGE_SIZE - 1);
    if (start_hint == 0) start_hint = 0xD0000000; // default: above initial kernel regions
    start_hint = (start_hint + PAGE_SIZE -1) & ~(PAGE_SIZE - 1);

    // walk the line looking for gaps
    uint32_t candidate = start_hint;
    vmm_region_t* region = space->regions;

    while (region) {
        uint32_t region_end = region->base + region->size;

        // if candidate overlaps this region, skip it
        if (candidate >= region->base && candidate < region_end) {
            candidate = region_end;
            candidate = (candidate + PAGE_SIZE -1) & ~(PAGE_SIZE - 1);
        }

        // check if there's enough room between here and next region
        if (candidate + size <= (region->next ? region->next->base : 0xFFFFF000)) {
            return candidate;
        }
        region = region->next;
    }

    // ran out of address space, on 32-bit that's 4GB total, minus kernel regions, so this shouldn't happen unless something is way off
    kprintf("VMM: no free region of size 0x%x found\n", size);
    return 0;
}

vmm_region_t* vmm_find_region(vmm_address_space_t* space, uint32_t vaddr) {
    if (!space) return NULL;

    vmm_region_t* region = space->regions;
    while (region) {
        if (vaddr >= region->base && vaddr < region->base + region->size) {
            return region;
        }
        region = region->next;
    }
    return NULL;
}

int vmm_is_mapped(uint32_t vaddr) {
    return paging_get_physical(vaddr) != 0;
}

void vmm_dump_regions(vmm_address_space_t* space) {
    if (!space) return;

    // names for each region type so output is readable
    const char* type_names[] = {"FREE", "KCODE", "KDATA", "KHEAP", "KSTACK", "IDMAP", "UCODE", "UDATA", "UHEAP", "USTACK", "MMIO"};

    kprintf("VMM: --- Region Map (%u regions) ---\n", space->region_count);
    vmm_region_t* r = space->regions;
    while (r) {
        const char* name = (r->type <= REGION_MMIO ? type_names[r->type] : "???");
        kprintf(" 0x%x - 0x%x  [%s] %s%s%s\n", r->base, r->base + r->size, name, (r->flags & VMM_READ) ? "R" : "-", (r->flags & VMM_WRITE) ? "W" : "-", (r->flags & VMM_USER) ? "U" : "K");
        r = r->next;
    }
    kprintf("VMM: --- End Region Map ---\n");
}

