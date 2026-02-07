#include "kheap.h"
#include "pmm.h"
#include "paging.h"
#include "kprintf.h"

// Simple block header for tracking allocations
typedef struct block_header {
    uint32_t size; // Size of block not including header
    uint32_t is_free; // 1 = free, 0 = allocated
    struct block_header* next; // Next block in list
} block_header_t;

#define HEADER_SIZE sizeof(block_header_t)

// Heap boundaries
static uint32_t heap_start = 0;
static uint32_t heap_end = 0;
static uint32_t heap_max = 0;

// Frist block in our free list
static block_header_t* first_block = NULL;

// Stats
static uint32_t total_allocated = 0;

// Initial heap size: 1MB, max: 16MB
#define INITIAL_HEAP_SIZE (1024 * 1024)
#define MAX_HEAP_SIZE (16 * 1024 * 1024)

// Heap starts at 16MB virtual address (above identity mapped region)
#define HEAP_START_ADDR 0xC0400000

void kheap_init(void) {
    heap_start = HEAP_START_ADDR;
    heap_end = heap_start;
    heap_max = heap_start + MAX_HEAP_SIZE;

    // Allocate initial heap pages
    for (uint32_t i = 0; i < INITIAL_HEAP_SIZE; i += PAGE_SIZE) {
        void* frame = pmm_alloc_frame();
        if (!frame) {
            kprintf("Heap: Failed to allocate frame\n");
            return;
        }
        paging_map_page(heap_start + i, (uint32_t)frame, PAGE_PRESENT | PAGE_WRITE);
    }
    heap_end = heap_start + INITIAL_HEAP_SIZE;

    // initialize first block as one big free block
    first_block = (block_header_t*)heap_start;
    first_block->size = INITIAL_HEAP_SIZE - HEADER_SIZE;
    first_block->is_free = 1;
    first_block->next = NULL;

    kprintf("Heap: Initialized at 0x%x, size %u KB\n", heap_start, INITIAL_HEAP_SIZE / 1024);
}

// Expand heap by allocating more pages
static int heap_expand(uint32_t min_size) {
    uint32_t expand_size = (min_size + PAGE_SIZE -1) & ~(PAGE_SIZE - 1);

    if (heap_end + expand_size > heap_max) {
        return 0;
    }

    for (uint32_t i = 0; i < expand_size; i+= PAGE_SIZE) {
        void* frame = pmm_alloc_frame();
        if (!frame) {
            return 0;
        }
        paging_map_page(heap_end + i, (uint32_t)frame, PAGE_PRESENT | PAGE_WRITE);
    }

    // Find last block and extend it (or create new block)
    block_header_t* block = first_block;
    while (block->next) {
        block = block->next;
    }

    if (block->is_free) {
        // Extend last free block
        block->size += expand_size;
    } else {
        // Create new free block at end
        block_header_t* new_block = (block_header_t*)heap_end;
        new_block->size = expand_size - HEADER_SIZE;
        new_block->is_free = 1;
        new_block->next = NULL;
        block->next = new_block;
    }

    heap_end += expand_size;
    return 1;
}

// Find free block that fits
static block_header_t* find_free_block(size_t size) {
    block_header_t* block = first_block;

    while (block) {
        if (block->is_free && block->size >= size) {
            return block;
        }
        block = block->next;
    }
    return NULL;
}

// Split a block if it's much larger than needed
static void split_block(block_header_t* block, size_t size) {
    // Only split if remaining space is worth it
    if (block->size >= size + HEADER_SIZE + 32) {
        block_header_t* new_block = (block_header_t*)((uint8_t*)block + HEADER_SIZE + size);
        new_block->size = block->size - size - HEADER_SIZE;
        new_block->is_free = 1;
        new_block->next = block->next;

        block->size = size;
        block->next = new_block;
    }
}


void* kmalloc(size_t size) {
    if (size == 0) return NULL;

    // Align size to 8 bytes
    size = (size + 7) & ~7;

    block_header_t* block = find_free_block(size);

    // No suitable block, try to expand heap
    if (!block) {
        if (!heap_expand(size + HEADER_SIZE)) {
            kprintf("Heap: Out of memory!\n");
            return NULL;
        }
        block = find_free_block(size);
        if (!block) return NULL;
    }

    // Split block if too large
    split_block(block, size);

    block->is_free = 0;
    total_allocated += block->size;

    // Return pointer after header
    return (void*)((uint8_t*)block + HEADER_SIZE);
}

void* kmalloc_aligned(size_t size) {
    // Allocate extra space to ensure alignment
    void* ptr = kmalloc(size + PAGE_SIZE);
    if (!ptr) return NULL;
    
    // Align to page boundary
    uint32_t aligned = ((uint32_t)ptr + PAGE_SIZE - 1) & ~(PAGE_SIZE - 1);
    return (void*)aligned;
}

void* kcalloc(size_t num, size_t size) {
    size_t total = num * size;
    void* ptr = kmalloc(total);
    
    if (ptr) {
        // Zero the memory
        uint8_t* p = (uint8_t*)ptr;
        for (size_t i = 0; i < total; i++) {
            p[i] = 0;
        }
    }
    return ptr;
}

// Merge adjacent free blocks
static void merge_free_blocks(void) {
    block_header_t* block = first_block;
    
    while (block && block->next) {
        if (block->is_free && block->next->is_free) {
            // Merge with next block
            block->size += HEADER_SIZE + block->next->size;
            block->next = block->next->next;
            // Don't advance - check if we can merge again
        } else {
            block = block->next;
        }
    }
}

void kfree(void* ptr) {
    if (!ptr) return;

    // Get header
    block_header_t* block = (block_header_t*)((uint8_t*)ptr - HEADER_SIZE);
    
    if (block->is_free) {
        kprintf("Heap: Double free detected!\n");
        return;
    }

    block->is_free = 1;
    total_allocated -= block->size;

    // Merge adjacent free blocks
    merge_free_blocks();
}

uint32_t kheap_get_used(void) {
    return total_allocated;
}

uint32_t kheap_get_free(void) {
    return (heap_end - heap_start) - total_allocated;
}
