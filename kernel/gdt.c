// This is where we actually populate those GDT structs and tell the CPU to use them
#include "gdt.h"
#include "tss.h"

/*
* We define 3 segments:
*   [0] Null descriptor - CPU requires index 0 to be empty
*   [1] Kernel code - where the CPU fetches instructions from
*   [2] Kernel data - where the CPU reads/writes memory

* Both code and data segments span 0x00000000 to 0xFFFFFFFF (all 4GB)
* This is called "flat mode", segmentation is effectively disabled
* we'll use paging later for real memory protection
*/

// Our GDT with 6 entries (was 3)
struct gdt_entry gdt[6];

// Pointer we feed to the `lgdt` instruction
struct gdt_ptr gp;

// Defined in gdt_flush.asm, loads the GDT and reloads segment registers
extern void gdt_flush(uint32_t gdt_ptr_address);

/*
* Helper to set one GDT entry

* The base and limit are split across the struct in a messy way, so this function handles the bit-shuffling for us
* this took a lot of trial and error and I nearly kms

* Parameters:
*   index: which GDT slot (0, 1, or 2)
*   base: segment start address (0x00000000 for flat mode)
*   limit: -segment size (0xFFFFFFFF for flat mode = 4GB)
*   access: access byte (defines segment type and perms)
*   gran: granularity byte (flags + upper limit bits)
*/
static void gdt_set_entry(int index, uint32_t base, uint32_t limit, uint8_t access, uint8_t gran) {
    // Scatter the 32-bit base across three fields
    gdt[index].base_low = (base & 0xFFFF); // bits 0-15
    gdt[index].base_middle = (base >> 16) & 0xFF; // bits 16-23
    gdt[index].base_high = (base >> 24) & 0xFF; // bits 24-31

    // Scatter the 20-bit limit across two fields
    gdt[index].limit_low = (limit & 0xFFFF); // bits 0-15

    // Granularity byte: upper nibble has flags + limit bits 16-19
    gdt[index].granularity = (limit >> 16) & 0x0F; // limit bits 16-19
    gdt[index].granularity |= (gran & 0xF0); // flags in upper nibble

    gdt[index].access = access;
}
/* Example:
Input:
  base  = 0x12345678
  limit = 0x000FFFFF
  access = 0x9A
  gran   = 0xCF

The base (0x12345678) gets chopped into 3 pieces:
  base_low    = 0x12345678 & 0xFFFF     = 0x5678  (bits 0-15)
  base_middle = (0x12345678 >> 16) & 0xFF = 0x34   (bits 16-23)
  base_high   = (0x12345678 >> 24) & 0xFF = 0x12   (bits 24-31)

  Reassembled: 0x12 | 0x34 | 0x5678 = 0x12345678 ✓

The limit (0x000FFFFF) gets chopped into 2 pieces:
  limit_low   = 0x000FFFFF & 0xFFFF       = 0xFFFF (bits 0-15)
  granularity = (0x000FFFFF >> 16) & 0x0F  = 0x0F  (bits 16-19)
  granularity |= (0xCF & 0xF0)            = 0x0F | 0xC0 = 0xCF

  Reassembled limit: 0xF | 0xFFFF = 0xFFFFF ✓
  (with granularity bit set, 0xFFFFF * 4KB pages = 4GB)

Final struct in memory (8 bytes):
  [FF FF] [78 56] [34] [9A] [CF] [12]
   limit   base   base access gran base
   low     low    mid              high
*/

void gdt_init(void) {
    // Tell the CPU how big our GDT is and where it lives
    gp.limit = (sizeof(struct gdt_entry) * 6) - 1; // size minus 1
    gp.base = (uint32_t)&gdt; // address of the table


    // Entry 0: Null descriptor
    // If a segment register accidentally holds 0, the CPU will fault instead of silently using garbage memory. It's a safety net
    gdt_set_entry(0, 0, 0, 0, 0);

    /* Entry 1: Kernel code segment
    * Base: 0x00000000, Limit: 0xFFFFFFFF (entire 4GB address space)
    * Access byte: 0x9A = 1001 1010 in binary
    *   bit 7 = 1: Present - segment is valid/active
    *   bits 5-6 = 00: Ring 0, kernel privilege (most privileged)
    *   bit 4 = 1: Descriptor type, this is a code/data segment (not system)
    *   bit 3 = 1: Executable, this is a CODE segment
    *   bit 2 = 0: Direction, grows up
    *   bit 1 = 1: Readable, code can be read (not just executed)
    *   bit 0 = 0: Accessed, CPU sets this when segment is used

    * Granularity byte: 0xCF = 1100 1111
    *   bit 7   = 1  : Granularity — limit is in 4KB pages (not bytes), so 0xFFFFF pages * 4KB = 4GB total
    *   bit 6   = 1  : Size — 32-bit protected mode segment
    *   bit 5   = 0  : Reserved (must be 0)
    *   bit 4   = 0  : Reserved (must be 0)
    *   bits 0-3 = 0xF : Upper 4 bits of limit (0xFFFFF total)
    */
    gdt_set_entry(1, 0, 0xFFFFFFFF, 0x9A, 0xCF);

     /* Entry 2: Kernel Data Segment
     * Identical to code segment except:
     *
     * Access byte: 0x92 = 1001 0010
     *   bit 3   = 0: NOT executable, this is a DATA segment
     *   bit 1   = 1: Writable, data can be written to
     *
     * Everything else is the same: ring 0, present, covers all 4GB.
     */
     gdt_set_entry(2, 0, 0xFFFFFFFF, 0x92, 0xCF);

     gdt_set_entry(3, 0, 0xFFFFFFFF, 0xFA, 0xCF);

     gdt_set_entry(4, 0, 0xFFFFFFFF, 0xF2, 0xCF);


     extern tss_t* tss_get(void);
     uint32_t tss_base = (uint32_t)tss_get();
     uint32_t tss_limit = sizeof(tss_t) - 1;
     gdt_set_entry(5, tss_base, tss_limit, 0x89, 0x00);

     // Load the GDT into the CPU and reload all segment registers
     gdt_flush((uint32_t)&gp);
}