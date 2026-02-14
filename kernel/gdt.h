// The GDT tells the CPU about memory segments
// Where code and data live, and what privilege level can access them. GRUB sets up a temporary one, but we need our own
#ifndef GDT_H
#define GDT_H

#include <stdint.h>

/*
* The GDT is an x86 structure that defines memory segments.
* In the old days, segmentation was how you isolated programs.
* Modern OSes use "flat mode", segmnets that cover ALL memory
* (base=0, limit=4GB), and use paging for real protection instead

* But the CPU still REQUIRES a GDT, even if we make every segment cover everything.
* We need at minimum:
* - A null descriptor (index 0, required by CPU)
* - A kernel code segment (for executing instructions)
* - A kernel data segment (for reading/writing memory)

* Each entry is 8 bytes packed in a weird legacy format from the 80s, gg....
* Intel kept it backwards-compatible, so the fields are split across non-contiguous bits. That's why the struct looks odd
*/

/* A single GDT entry (8 bytes)
 *
 * The 32-bit base address and 20-bit limit are split across the struct:
 *   - limit_low:  bits 0-15 of the segment size
 *   - base_low:   bits 0-15 of the segment start address
 *   - base_middle: bits 16-23 of the segment start address
 *   - base_high:  bits 24-31 of the segment start address
 *   - granularity: contains bits 16-19 of the limit + flags
 *
 * Why so fragmented? Backwards compatibility with the 286 (1982).
 * Intel added 386 fields by stuffing extra bits into unused spots.
 * Yeah thanks intel....
 */

 // I EXPANDED THE GDT WHEN WRITING TSS JHUST NOTE THAT
 struct gdt_entry {
    uint16_t limit_low; /* Lower 16 bits of segment limit*/
    uint16_t base_low; /* Lower 16 bits of base address*/
    uint8_t base_middle; /* Next 8 bits of base address*/
    uint8_t access; /* Access flags: present, privilege, type*/
    uint8_t granularity; /* Flags + upper 4 bits of limit*/
    uint8_t base_high; /* Upper 8 bits of base address*/
 } __attribute__((packed)); /* packed = no padding between fields*/

 /* The GDT pointer struct, this is what the `lgdt` instruction reads
 * It needs to know the size of the table where it lives in memory
 */
 struct gdt_ptr {
    uint16_t limit; /* Total size of GDT in bytes, minus 1*/
    uint32_t base; /* Memory address where the GDT starts*/
 } __attribute__((packed));

 // Set up and install the GDT. Called once during boot
 void gdt_init(void);

 #endif