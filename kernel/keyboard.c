#include "keyboard.h"
#include "ports.h"
#include "idt.h"
#include "terminal.h"

/*
* Flow:
* 1. User presses a key
* 2. Keyboard controller sends IRQ 1 -> interrupt 33 after PIC remap
* 3. Our handler reads the scancode from port 0x60
* 4. We look up the scancode in a table to get the ASCII character
* 5. We print the character to the screen

* Scancodes aren't ASCII, they're arbitrary numbers assigned to physical key positions. 'A' is 0x1E, 'B' is 0x30, etc.
* The mapping has no logical pattern, it's based on the physical keyboard layout
*/

#define KEYBOARD_DATA_PORT 0x60

static const char scancode_to_ascii[128] = {
    0,
    0,
    '1',
    '2',
    '3',
    '4',
    '5',
    '6',
    '7',
    '8',
    '9',
    '0',
    '-',
    '=',
    '\b', // Backspace
    '\t', // Tab
    'q',
    'w',
    'e',
    'r',
    't',
    'y',
    'u',
    'i',
    'o',
    'p',
    '[',
    ']',
    '\n', // Enter
    0, // Left Ctrl
    'a',
    's',
    'd',
    'f',
    'g',
    'h',
    'j',
    'k',
    'l',
    ';',
    '\'',
    '`',
    0, // Left Shift
    '\\',
    'z',
    'x',
    'c',
    'v',
    'b',
    'n',
    'm',
    ',',
    '.',
    '/',
    0, // Right Shift
    '*',
    0, // Left Alt
    ' ', // Space
    0, // Caps Lock
    0, // F1
    0, // F2
    0, // F3
    0, // F4
    0, // F5
    0, // F6
    0, // F7
    0, // F8
    0, // F9
    0, // F10
    0, // Num Lock
    0, // Scroll Lock
    '7', // Keypad 7
    '8', // Keypad 8
    '9', // Keypad 9
    '-', // Keypad -
    '4', // Keypad 4
    '5', // Keypad 5
    '6', // Keypad 6
    '+', // Keypad +
    '1', // Keypad 1
    '2', // Keypad 2
    '3', // Keypad 3
    '0', // Keypad 0
    '.', // Keypad .
    0, 0, 0, // unused
    0, // F11
    0, // F12
    // Rest are unused or extended keys, leave as 0
};

static void keyboard_handler(struct interrupt_frame* frame) {
    (void)frame; // unused parameter to avoid compiler warning

    // Read the scancode from the keyboard controller
    uint8_t scancode = inb(KEYBOARD_DATA_PORT);

    // Ignore key releases
    if (scancode & 0x80) {
        return;
    }

    // Look up the ASCII char, and make sure scancode is in bounds before indexing
    if (scancode < 128) {
        char c = scancode_to_ascii[scancode];

        // If it's a printable char, print it
        if (c != 0) {
            terminal_putchar(c);
        }
    }
}

void keyboard_init(void) {
    // register our handler for IRQ 1 (interrupt 33 after PIC remap)
    idt_register_handler(KEYBOARD_IRQ, keyboard_handler);

    /* The keyboard is alread enabled by the BIOS, so we don't need to send any initialization commands. Just register the handler and we're good to go
    *  A fanicer driver would reset the keyboard, set the repeat rate, enable scanning, set LED state, but the defaults work fine for now
    */
}

