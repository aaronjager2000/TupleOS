#include "shell.h"
#include "terminal.h"
#include "timer.h"
#include "process.h"
#include "pmm.h"
#include "kheap.h"
#include "kprintf.h"
#include <stddef.h>
#include <stdint.h>


#define MAX_CMD_LENGTH 256

static char cmd_buffer[MAX_CMD_LENGTH];
static size_t cmd_length = 0;
static int foreground_pid = -1;

static int strcmp(const char* s1, const char* s2) {
    while (*s1 && (*s1 == *s2)) {
        s1++;
        s2++;
    }
    return *(unsigned char*)s1 - *(unsigned char*)s2;
}

static int startswith(const char* str, const char* prefix) {
    while (*prefix) {
        if (*str++ != *prefix++) return 0;
    }
    return 1;
}

static uint32_t parse_uint(const char* str) {
    uint32_t result = 0;
    while (*str >= '0' && *str <= '9') {
        result = result * 10 + (*str - '0');
        str++;
    }
    return result;
}

static void print_uint(uint32_t num) {
    if (num == 0) {
        terminal_putchar('0');
        return;
    }
    char buffer[12];
    int i = 0;
    while (num > 0) {
        buffer[i++] = '0' + (num % 10);
        num /= 10;
    }
    while (--i >= 0) {
        terminal_putchar(buffer[i]);
    }
}

// pad a number to at least `width` characters (right-aligned)
static void print_uint_padded(uint32_t num, int width) {
    char buffer[12];
    int len = 0;

    if (num == 0) {
        buffer[len++] = '0';
    } else {
        while (num > 0) {
            buffer[len++] = '0' + (num % 10);
            num /= 10;
        }
    }

    // pad with spaces
    for (int i = 0; i < width - len; i++) {
        terminal_putchar(' ');
    }
    // print digits in correct order
    for (int i = len - 1; i >= 0; i--) {
        terminal_putchar(buffer[i]);
    }
}

// pad a string to at least `width` characters (left-aligned)
static void print_str_padded(const char* str, int width) {
    int len = 0;
    const char* s = str;
    while (*s++) len++;

    terminal_writestring(str);
    for (int i = len; i < width; i++) {
        terminal_putchar(' ');
    }
}

static void shell_prompt(void) {
    terminal_writestring("> ");
}

// ============================================================
// COMMANDS
// ============================================================

static void cmd_help(void) {
    terminal_writestring("Available commands:\n");
    terminal_writestring("  help           - Show this message\n");
    terminal_writestring("  clear          - Clear the screen\n");
    terminal_writestring("  ticks          - Timer ticks since boot\n");
    terminal_writestring("  about          - About TupleOS\n");
    terminal_writestring("  ps             - List all processes\n");
    terminal_writestring("  run <name>     - Spawn a test program\n");
    terminal_writestring("  kill <pid>     - Kill a process\n");
    terminal_writestring("  meminfo        - Memory usage stats\n");
    terminal_writestring("  programs       - List available test programs\n");
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

static void cmd_ps(void) {
    kprintf("PID  %-16s STATE\n", "NAME");
    for (int i = 0; i < MAX_PROCESSES; i++) {
        process_t* proc = process_get_by_slot(i);
        if (!proc) continue;
        print_uint_padded(proc->pid, 4);
        terminal_putchar(' ');
        print_str_padded(proc->name, 17);
        terminal_writestring(process_state_name(proc->state));
        terminal_putchar('\n');
    }
}

static void cmd_run(const char* name) {
    // skip leading whitespace
    while (*name == ' ') name++;

    if (*name == '\0') {
        terminal_writestring("Usage: run <name>\n");
        terminal_writestring("Type 'programs' to see available test programs\n");
        return;
    }

    int pid = process_spawn_test(name);
    if (pid < 0) {
        kprintf("Unknown program: %s\n", name);
        terminal_writestring("Type 'programs' to see available test programs\n");
    } else {
        kprintf("Started '%s' (PID %d)\n", name, pid);
        foreground_pid = pid;
    }
}

static void cmd_kill(const char* arg) {
    while (*arg == ' ') arg++;

    if (*arg == '\0' || *arg < '0' || *arg > '9') {
        terminal_writestring("Usage: kill <pid>\n");
        return;
    }

    uint32_t pid = parse_uint(arg);
    int ret = process_kill(pid);
    if (ret < 0) {
        kprintf("Failed to kill PID %u (not found or can't be killed)\n", pid);
    } else {
        kprintf("Killed PID %u\n", pid);
    }
}

static void cmd_meminfo(void) {
    uint32_t phys_free = pmm_get_free_memory();
    uint32_t phys_total = pmm_get_total_memory();
    uint32_t heap_used = kheap_get_used();
    uint32_t heap_free = kheap_get_free();

    // count active processes
    int proc_count = 0;
    for (int i = 0; i < MAX_PROCESSES; i++) {
        if (process_get_by_slot(i)) proc_count++;
    }

    kprintf("Physical memory: %u KB free / %u KB total\n", phys_free / 1024, phys_total / 1024);
    kprintf("Kernel heap:     %u KB used / %u KB free\n", heap_used / 1024, heap_free / 1024);
    kprintf("Processes:       %d active\n", proc_count);
}

static void cmd_programs(void) {
    terminal_writestring("Available test programs:\n");
    for (int i = 0; ; i++) {
        const char* name = process_get_test_name(i);
        if (!name) break;
        terminal_writestring("  ");
        terminal_writestring(name);
        terminal_putchar('\n');
    }
}

void shell_init(void) {
    terminal_writestring("Welcome To The TupleOS Shell\n");
    terminal_writestring("Type 'help' for a list of commands\n");
    shell_prompt();
}


static void shell_execute(void) {
    cmd_buffer[cmd_length] = '\0';

    // skip empty input
    if (cmd_length == 0) {
        shell_prompt();
        return;
    }

    if (strcmp(cmd_buffer, "help") == 0) {
        cmd_help();
    } else if (strcmp(cmd_buffer, "clear") == 0) {
        cmd_clear();
    } else if (strcmp(cmd_buffer, "about") == 0) {
        cmd_about();
    } else if (strcmp(cmd_buffer, "ticks") == 0) {
        cmd_ticks();
    } else if (strcmp(cmd_buffer, "ps") == 0) {
        cmd_ps();
    } else if (startswith(cmd_buffer, "run ") || strcmp(cmd_buffer, "run") == 0) {
        cmd_run(cmd_buffer + 3);
    } else if (startswith(cmd_buffer, "kill ") || strcmp(cmd_buffer, "kill") == 0) {
        cmd_kill(cmd_buffer + 4);
    } else if (strcmp(cmd_buffer, "meminfo") == 0) {
        cmd_meminfo();
    } else if (strcmp(cmd_buffer, "programs") == 0) {
        cmd_programs();
    } else {
        terminal_writestring("Unknown command: ");
        terminal_writestring(cmd_buffer);
        terminal_putchar('\n');
    }

    cmd_length = 0;
    if (foreground_pid < 0) {
        shell_prompt();
    }
}

void shell_check_foreground(void) {
    if (foreground_pid < 0) return;
    process_t* proc = process_get((uint32_t)foreground_pid);
    if (!proc || proc->state == PROCESS_ZOMBIE) {
        foreground_pid = -1;
        shell_prompt();
    }
}

void shell_handle_key(char c) {
    shell_check_foreground();
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
