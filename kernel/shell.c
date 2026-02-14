#include "shell.h"
#include "terminal.h"
#include "timer.h"
#include <stddef.h>
#include <stdint.h>


#define MAX_CMD_LENGTH 256

static char cmd_buffer[MAX_CMD_LENGTH];
static size_t cmd_length = 0;

static int strcmp(const char* s1, const char* s2) {
    while (*s1 && (*s1 == *s2)) {
        s1++;
        s2++;
    }
    return *(unsigned char*)s1 - *(unsigned char*)s2;
}

static void print_uint(uint32_t num) {
    if (num == 0) {
        terminal_putchar('0');
        return;
    }
    char buffer[12]; // enough for 32-bit int
    int i = 0;
    while (num > 0) {
        buffer[i++] = '0' + (num % 10);
        num /= 10;
    }
    while (--i >= 0) {
        terminal_putchar(buffer[i]);
    }
}


static void shell_prompt(void) {
    terminal_writestring("> ");
}

// COMMAND IMPLEMENTATIONS

static void cmd_help(void) {
    terminal_writestring("Available commands:\n");
    terminal_writestring("help - Show this message\n");
    terminal_writestring("clear - Clear the screen\n");
    terminal_writestring("ticks - Show number of timer ticks since boot\n");
    terminal_writestring("about - About TupleOS\n");
}

static void cmd_clear(void) {
    terminal_initialize();
}

static void cmd_about(void) {
    terminal_writestring("TupleOS v0.1\n");
    terminal_writestring("A simple hobby OS written in C\n");
    terminal_writestring("Created by Aaron Grant and Val Petrov\n");
}

static void cmd_ticks(void) {
    terminal_writestring("Ticks since boot: ");
    print_uint(timer_get_ticks());
    terminal_putchar('\n');
}

void shell_init(void){
    terminal_writestring("Welcome To The TupleOS Shell\n");
    terminal_writestring("Type 'help' for a list of commands\n");
    shell_prompt();
}

typedef void (*cmd_fn)(void);

typedef struct {
    const char* name;
    cmd_fn fn;
} command_t;

static const command_t commands[] = {
    { "help",  cmd_help  },
    { "clear", cmd_clear },
    { "about", cmd_about },
    { "ticks", cmd_ticks },
};

static void shell_execute(void) {
    cmd_buffer[cmd_length] = '\0';

    for (size_t i = 0; i < sizeof(commands) / sizeof(commands[0]); ++i) {
        if (strcmp(cmd_buffer, commands[i].name) == 0) {
            commands[i].fn();
            cmd_length = 0;
            shell_prompt();
            return;
        }
    }

    terminal_writestring("Unknown command: ");
    terminal_writestring(cmd_buffer);
    terminal_putchar('\n');

    cmd_length = 0;
    shell_prompt();
}

void shell_handle_key(char c) {
    if (c == '\n') {
        terminal_putchar('\n');
        shell_execute();
    } else if (c == '\b') {
        if (cmd_length > 0) {
            cmd_length--;
            terminal_putchar('\b');
        }
    } else {
        if (cmd_length < MAX_CMD_LENGTH - 1) {
            cmd_buffer[cmd_length++] = c;
            terminal_putchar(c);
        }
    }
}