#ifndef KEYBOARD_H
#define KEYBOARD_H

/* PS/2 Keyboard Driver
*  The keyboard sends scancodes (one byte per press/release) via IRQ 1.
*  We read the scancode from port 0x60 and translate it to ASCII.

*  PS/2 keyboards use "Scan Code Set 1" by default. Each key has a unique code when pressed, and the same code + 0x80 when released.
*  Ex:
*  'A' pressed = 0x1E
*  'A' released = 0x9E (0x1E + 0x80)
*/

#define KEYBOARD_IRQ 33

// Init the keyboard driver (which will register the IRQ handler)
void keyboard_init(void);

#endif