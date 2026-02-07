#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include "terminal.h"
#include "keyboard.h"
#include "gdt.h"
#include "idt.h"
#include "timer.h"
#include "shell.h"
#include "ports.h"
#include "kprintf.h"
#include "serial.h"
#include "multiboot.h"
#include "pmm.h"
#include "paging.h"
#include "kheap.h"
#include "vmm.h"

// Make good comments, and good commits


static inline uint16_t vga_entry(unsigned char uc, uint8_t color) {
    return (uint16_t)uc | (uint16_t)color << 8;
}

size_t strlen(const char* str) {
    size_t len = 0;
    while (str[len]) {
        len++;
    }
    return len;
}

// VGA text mode buffer constants
static const size_t VGA_WIDTH = 80;
static const size_t VGA_HEIGHT = 25;

// Global variables tracking terminal state
// Global because we have no object system, no state management. Simple approach is global state
size_t terminal_row; // current row (0-24)
size_t terminal_column; // current column (0-79)
uint8_t terminal_color; // current color (for next character)
uint16_t* terminal_buffer; // pointer to VGA memory

void terminal_initialize(void) {
    // start cursor at top-left corner
    terminal_row = 0;
    terminal_column = 0;
    // set default color to light gray text on black background
    terminal_color = vga_entry_color(VGA_COLOR_LIGHT_GREY, VGA_COLOR_BLACK);
    // This is the magic line. 0xB8000 is a physical memory address where VGA text buffer lives in RAM. When you write to this address, it apprears on screen instantly. No OS api, no drivers just raw hardware access babyyyy.
    // Cast to uint16_t* because each character cell is 2 bytes (1 byte for ASCII char, 1 byte for color)
    terminal_buffer = (uint16_t*)0xC00B8000; // VGA memory at 0xB8000, mapped in higher half

    // Clear the screen by writing spaces to all character cells
    // The buffer is a 1D array of 2000 entries, but we think of it as 2D (25 rows x 80 columns)
    // To convert (x, y) to array index -> index = y * VGA_WIDTH + x
    for (size_t y = 0; y < VGA_HEIGHT; y++) {
        for (size_t x = 0; x < VGA_WIDTH; x++) {
            const size_t index = y * VGA_WIDTH + x;
            terminal_buffer[index] = vga_entry(' ', terminal_color);
        }
    }
}

void terminal_setcolor(uint8_t color) {
    terminal_color = color;
}

void terminal_update_cursor(void) {
    uint16_t pos = terminal_row * VGA_WIDTH + terminal_column;

    // The VGA hardware cursor is controlled by IO ports 0x3D4 and 0x3D5
    // 0x3D4 = control register, 0x3D5 = data register

    outb(0x3D4, 0x0F); // Tell VGA we want to set the low byte of the cursor position
    outb(0x3D5, (uint8_t)(pos & 0xFF)); // Send the low byte of the position
    outb(0x3D4, 0x0E); // Tell VGA we want to set the high byte of the cursor position
    outb(0x3D5, (uint8_t)((pos >> 8) & 0xFF)); // Send the high byte of the position
}

// function to put single character at (x, y) with given color
void terminal_putentryat(char c, uint8_t color, size_t x, size_t y) {
    const size_t index = y * VGA_WIDTH + x;
    terminal_buffer[index] = vga_entry(c, color);
}

// Scroll the entire screen up by one line
// Copy every line up by one row, then clear the last row (fill with spaces)
void terminal_scroll(void) {
    // Move each row up by one: copy row 1 to row 0, row 2 to row 1, etc.
    for (size_t y = 0; y < VGA_HEIGHT - 1; y++) {
        for (size_t x = 0; x < VGA_WIDTH; x++) {
            const size_t dst = y * VGA_WIDTH + x; // destination: current row
            const size_t src = (y + 1) * VGA_WIDTH + x; // source: next row down
            terminal_buffer[dst] = terminal_buffer[src];
        }
    }
    // Clear last row
    for (size_t x = 0; x < VGA_WIDTH; x++) {
        const size_t index = (VGA_HEIGHT - 1) * VGA_WIDTH + x;
        terminal_buffer[index] = vga_entry(' ', terminal_color);
    }
}

void terminal_putchar(char c) {
    // Handle backspace: move cursor back and erase char
    if (c == '\b') {
        if (terminal_column > 0) {
            terminal_column--;
        } else if (terminal_row > 0) {
            terminal_row--;
            terminal_column = VGA_WIDTH - 1;
        }
        // Erase the char at the new cursor pos
        terminal_putentryat(' ', terminal_color, terminal_column, terminal_row);
        terminal_update_cursor();
        return;
    }

    // Handle newline: move to the beginning of the next line
    // instead of trying to display '\n' as a visible char
    if (c == '\n') {
        terminal_column = 0;
        if (++terminal_row == VGA_HEIGHT) {
            terminal_scroll(); 
            terminal_row = VGA_HEIGHT - 1;
        }
        return;
    }

    terminal_putentryat(c, terminal_color, terminal_column, terminal_row);
    // After placing a char, advance the cursor, wrapping to next line if needed
    if (++terminal_column == VGA_WIDTH) {
        terminal_column = 0;
        if (++terminal_row == VGA_HEIGHT) {
            terminal_scroll();
            terminal_row = VGA_HEIGHT - 1;
        }
    }
    terminal_update_cursor();
}

// Prints multiple characters
void terminal_write(const char* data, size_t size) {
    for (size_t i = 0; i< size; i++) {
        terminal_putchar(data[i]);
    }
}

// Convenient function for strings
void terminal_writestring(const char* data) {
    terminal_write(data, strlen(data));
}

static void terminal_print_int(uint32_t num) {
    if (num == 0) {
        terminal_putchar('0');
        return;
    }

    char buffer[12]; // enough for 32-bit int
    int i = 0;

    // Convert integer to string in reverse order
    while (num > 0) {
        buffer[i++] = '0' + (num % 10);
        num /= 10;
    }

    // Print the string in correct order
    for (int j = i - 1; j >= 0; j--) {
        terminal_putchar(buffer[j]);
    }
}

void kernel_main(uint32_t magic, multiboot_info_t* mbi) {
    terminal_initialize();
    serial_init();
    serial_printf("Serial port initialized\n");
    serial_printf("Timer ticks: %u\n", timer_get_ticks());
    terminal_writestring("Initalizing GDT\n");
    gdt_init();
    terminal_writestring("GDT Initialized\n");
    terminal_writestring("Initalizing IDT\n");
    idt_init();
    terminal_writestring("IDT Initialized\n");
    terminal_writestring("Initializing Keyboard\n");
    keyboard_init();
    terminal_writestring("Keyboard Initialized\n");
    terminal_writestring("Kernel initialization complete!\n");
    timer_init(100);
    kprintf("Timer initialized at 100 Hz\n");

    // Verify multiboot magic and initialize PMM
    if (magic != MULTIBOOT_MAGIC) {
        kprintf("ERROR: Invalid multiboot magic: 0x%x\n", magic);
    } else {
        kprintf("Multiboot magic OK: 0x%x\n", magic);
        pmm_init(mbi);
        paging_init();
        kheap_init();
        vmm_init();
        kprintf("Heap used: %u KB, free: %u KB\n", kheap_get_used() / 1024, kheap_get_free() / 1024);
        kprintf("Free memory: %u KB\n", pmm_get_free_memory() / 1024);
    }

    kprintf("Welcome to TupleOS!\n");
    shell_init();

    while (1) {
        __asm__ volatile("hlt");
    }
}


// Entry point from boot.asm
// 1: Initialize terminal (clear screen, set up state)
// 2: Print message
// Then the function ends, returns to boot.asm which executes the hang loop (cli; hlt; jmp)

// the flow: kernel_main() -> terminal_initialize() -> terminal_writestring() -> terminal_write() -> terminal_putchar() -> terminal_putentryat() -> VGA hardware displays it on the monitor