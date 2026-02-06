#ifndef TERMINAL_H
#define TERMINAL_H

#include <stddef.h>
#include <stdint.h>

/*
* Functions for writing to the screen via the VGA text buffer
* Actual impleemntation lives in kernel.c for now
*/

enum vga_color {
    VGA_COLOR_BLACK = 0,
    VGA_COLOR_BLUE = 1,
    VGA_COLOR_GREEN = 2,
    VGA_COLOR_CYAN = 3,
    VGA_COLOR_RED = 4,
    VGA_COLOR_MAGENTA = 5,
    VGA_COLOR_BROWN = 6,
    VGA_COLOR_LIGHT_GREY = 7,
    VGA_COLOR_DARK_GREY = 8,
    VGA_COLOR_LIGHT_BLUE = 9,
    VGA_COLOR_LIGHT_GREEN = 10,
    VGA_COLOR_LIGHT_CYAN = 11,
    VGA_COLOR_LIGHT_RED = 12,
    VGA_COLOR_LIGHT_MAGENTA = 13,
    VGA_COLOR_LIGHT_BROWN = 14,
    VGA_COLOR_WHITE = 15,
};

// Initialize the terminal (clears screen, resets cursor) 
void terminal_initialize(void);

// Set the color for subsequent output 
void terminal_setcolor(uint8_t color);

// Print a single character 
void terminal_putchar(char c);

// Print a buffer of known length 
void terminal_write(const char* data, size_t size);

// Print a null-terminated string 
void terminal_writestring(const char* data);

// Helper to create a color byte from foreground and background 
static inline uint8_t vga_entry_color(enum vga_color fg, enum vga_color bg) {
    return fg | bg << 4;
}

#endif