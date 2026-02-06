#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include "terminal.h"
#include "keyboard.h"
#include "gdt.h"
#include "idt.h"

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
    terminal_buffer = (uint16_t*)0xB8000; // VGA memory address

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

// function to put single character at (x, y) with given color
void terminal_putentryat(char c, uint8_t color, size_t x, size_t y) {
    const size_t index = y * VGA_WIDTH + x;
    terminal_buffer[index] = vga_entry(c, color);
}

// Scroll the entire screen up by one line
// Copy every line up by one row, then clear the last row (fill with spaces)
// Since we're writing to 0xB8000, the screen updates instantly as we copy
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

void kernel_main(void) {
    terminal_initialize();
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
}

// Entry point from boot.asm
// 1: Initialize terminal (clear screen, set up state)
// 2: Print message
// Then the function ends, returns to boot.asm which executes the hang loop (cli; hlt; jmp)

// the flow: kernel_main() -> terminal_initialize() -> terminal_writestring() -> terminal_write() -> terminal_putchar() -> terminal_putentryat() -> VGA hardware displays it on the monitor