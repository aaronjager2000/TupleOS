#include "timer.h"
#include "ports.h"
#include "idt.h"
#include "scheduler.h"
#include "shell.h"

/* The PIT has 3 channels. We only care about channel 0 though, which is connected to IRQ 0
*  We will program it to fire at a fixed frequency
* PIT ports:
* 0x40 = Channel 0 data port
* 0x43 = Command/mode register

* The PIT runs at 1,193,182 Hz. (this is legacy from the original IBM PC)
* To get a desired frequency, we set a "divisor":
* divisor = 1193182 / frequency

* The PIT counts down from the divisor to zero, then fires IRQ 0 and reloads the divisor
*/

#define PIT_FREQUENCY 1193182

#define PIT_CHANNEL0 0x40
#define PIT_COMAND 0x43

// Gloabl tick counter incremented every time IRQ 0 fires
static volatile uint32_t ticks = 0;

static void timer_handler(struct interrupt_frame* frame) {
    (void)frame; // unused parameter to avoid compiler warning
    ticks++;
    // Need to send EOI BEFORE!! schedule() because context_switch may never return to this stack frame, it switches to another process's stakc, and interrupt_handler's EOI code (in idt.c) won't execute until this process gets scheduled again
    // Without EOI, the PIC blocks all futer timer interrupts
    outb(0x20, 0x20); // EOI to master PIC (timer is IRQ 0 = master only)
    scheduler_wake_sleepers(ticks); // wake any processes whose sleep expired
    schedule();
    shell_check_foreground(); // re-prompt if foreground process exited
}

uint32_t timer_get_ticks(void) {
    return ticks;
}

void timer_init(uint32_t frequency) {
    uint32_t divisor = PIT_FREQUENCY / frequency;

    if (divisor > 65535) {
        divisor = 65535; // max divisor for 16-bit counter ~ 18 Hz
    }

    if (divisor < 1) {
        divisor = 1; // min divisor for ~ 1.19 MHz
     }
    
     // Send the command byte to the PIT
    
    outb(PIT_COMAND, 0x36); // 0x36 = 0011 0110

    // Send the divisor low byte first, then high byte
    outb(PIT_CHANNEL0, (uint8_t)(divisor & 0xFF)); // low byte
    outb(PIT_CHANNEL0, (uint8_t)((divisor >> 8) & 0xFF)); // high byte

    // Resgister our handler for IRQ 0 (interrupt 32)
    idt_register_handler(TIMER_IRQ, timer_handler);


}