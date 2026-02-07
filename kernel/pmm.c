#include "pmm.h"
#include "kprintf.h"


// Bit map to track page frames (1 bit per 4KB page)
// We'll place the bitmap at a fixed location after the kernel
// For now, support up to 128MB of RAM (32768 pages = 4096 bytes bitmap)
#define MAX_FRAMES 32786
static uint8_t frame_bitmap[MAX_FRAMES / 8];

// Stats
static uint32_t total_frames = 0;
static uint32_t used_frames = 0;

// Kernel end addr (defined in linker)
extern uint32_t _kernel_end;

// Helper to set a frame as used
static void bitmap_set(uint32_t frame) {
    frame_bitmap[frame / 8] |= (1 << (frame % 8));
}

// Helper to set a frame as free
static void bitmap_clear(uint32_t frame) {
    frame_bitmap[frame / 8] &= ~(1 << (frame % 8));
}

// Helper to check if frame is used
static int bitmap_test(uint32_t frame) {
    return frame_bitmap[frame / 8] & (1 << (frame % 8));
}

// Find first free frame
static int bitmap_first_free(void) {
    for (uint32_t i = 0; i < total_frames / 8; i++) {
        if (frame_bitmap[i] != 0xFF) {
            // At least one bit is free in this byte
            for (int j = 0; j < 8; j++) {
                if (!(frame_bitmap[i] & (1 << j))) {
                    return i * 8 + j;
                }
            }
        }
    }
    return -1; // No free frames
}

void pmm_init(multiboot_info_t* mbi) {
    // First, mark ALL frames as used (safe default)
    for (uint32_t i = 0; i < MAX_FRAMES / 8; i++) {
        frame_bitmap[i] = 0xFF;
    }

    // Check if memory map is available
    if (!(mbi->flags & MULTIBOOT_FLAG_MMAP)) {
        kprintf("PMM: No memory map available\n");
        return;
    }

    kprintf("PMM: Parsing memory map...\n");

    // _kernel_end is a virtual address(0xC01xxxxx) - convert to physical
    // for comparison with physical frame addresses
    uint32_t kernel_end = ((uint32_t)&_kernel_end - 0xC0000000 + PAGE_SIZE - 1) & ~(PAGE_SIZE - 1);

    // Parse memory map and mark available regions as free
    multiboot_mmap_entry_t* mmap = (multiboot_mmap_entry_t*)mbi->mmap_addr;
    uint32_t mmap_end = mbi->mmap_addr + mbi->mmap_length;

    while ((uint32_t)mmap < mmap_end) {
        if (mmap->type == MULTIBOOT_MEMORY_AVAILABLE) {
            // Calculate frame range for this region
            uint32_t base = (uint32_t)mmap->base_addr;
            uint32_t length = (uint32_t)mmap->length;

            kprintf("Available: 0x%x - 0x%x (%u KB)\n", base, base + length, length / 1024);

            // Align base up to page boundary
            uint32_t start_frame = (base + PAGE_SIZE - 1) / PAGE_SIZE;
            uint32_t end_frame = (base + length) / PAGE_SIZE;

            // Mark frames as free (but skip low memory and kernel)
            for (uint32_t frame = start_frame; frame < end_frame && frame < MAX_FRAMES; frame++) {
                uint32_t addr = frame * PAGE_SIZE;

                // Skip first 1MB (BIOS, VGA, etc.) and kernel memory
                if (addr < 0x100000 || addr < kernel_end) {
                    continue;
                }

                bitmap_clear(frame);
                total_frames++;
            }
        }

        // Move to next entry (size doesn't include the size field itself)
        mmap = (multiboot_mmap_entry_t*)((uint32_t)mmap + mmap->size + sizeof(mmap->size));
    }

    kprintf("PMM: %u frame (%u MB) available\n", total_frames, (total_frames * PAGE_SIZE) / (1024 * 1024));
}

void* pmm_alloc_frame(void) {
    int frame = bitmap_first_free();
    if (frame < 0) {
        kprintf("PMM: Out of memory!\n");
        return NULL;
    }

    bitmap_set(frame);
    used_frames++;

    return (void*)(frame * PAGE_SIZE);
}

void pmm_free_frame(void* frame_addr) {
    uint32_t frame = (uint32_t)frame_addr / PAGE_SIZE;

    if (frame >= MAX_FRAMES) {
        return;
    }

    if (bitmap_test(frame)) {
        bitmap_clear(frame);
        used_frames--;
    }
}

uint32_t pmm_get_total_memory(void) {
    return total_frames * PAGE_SIZE;
}

uint32_t pmm_get_free_memory(void) {
    return (total_frames - used_frames) * PAGE_SIZE;
}