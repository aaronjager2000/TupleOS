# Build system for TupleOS
# Make is a build automation tool. Instead of typing long compiler commands
# every time, you define rules here and just type "make" to build everything.
# Make is also smart: it only rebuilds files that have changed.
#
# HOW TO USE:
#   make          -> builds the kernel and creates a bootable ISO
#   make clean    -> deletes all generated files (start fresh)
#   make run      -> builds everything and launches QEMU to test
#
# TOOLCHAIN:
# We use a "cross-compiler" (i686-elf-gcc) instead of your system's gcc.
# Why? Your system's gcc targets YOUR operating system (Linux/Windows).
# We need a compiler that targets a "bare metal" x86 system with no OS.
# The i686-elf target means: 32-bit x86, ELF binary format, no OS assumptions.

# TOOL DEFINITIONS 
# AS = assembler (turns .asm into .o object files)
# CC = C compiler (turns .c into .o object files)  
# LD = linker (combines .o files into the final kernel binary)
# Using i686-elf cross-compiler toolchain for bare metal x86
AS = i686-elf-as
CC = i686-elf-gcc
LD = i686-elf-gcc

# COMPILER FLAGS 
# -ffreestanding: tells GCC this code runs without an OS
#    - won't assume standard library exists
#    - won't assume main() is the entry point
#    - only guarantees headers: <stddef.h>, <stdint.h>, <stdbool.h>, <stdarg.h>
#      (these are "freestanding headers" - they define types, not functions)
# -O2: optimization level 2 (good balance of speed and debuggability)
# -Wall: enable all common warnings (catch potential bugs)
# -Wextra: enable extra warnings beyond -Wall
# -nostdlib: don't link the standard C library (there is no libc on bare metal)
# -fno-builtin: don't use GCC's built-in function replacements
#    (GCC sometimes replaces calls like strlen() with its own optimized version,
#     but those might depend on libc which we don't have)
# -fno-stack-protector: disable stack smashing protection
#    (this feature requires __stack_chk_fail which is a libc function we don't have)
CFLAGS = -ffreestanding -O2 -Wall -Wextra -nostdlib -fno-builtin -fno-stack-protector

# LINKER FLAGS 
# -nostdlib: don't link standard libraries
# -T linker.ld: use our custom linker script to control memory layout
LDFLAGS = -nostdlib -T linker.ld

# FILE PATHS 
# Where compiled object files go
BUILD_DIR = build
# Where the ISO directory structure is assembled
ISO_DIR = iso
# The final kernel binary name
KERNEL = $(BUILD_DIR)/tupleos.bin
# The final bootable ISO image
ISO = $(BUILD_DIR)/tupleos.iso

# OBJECT FILES 
# These are the compiled versions of our source files
# boot.o comes from boot.asm, kernel.o comes from kernel.c
# IMPORTANT: boot.o must be listed FIRST so the multiboot header
# ends up at the very beginning of the binary (within the first 8KB)
OBJS = $(BUILD_DIR)/boot.o $(BUILD_DIR)/kernel.o

# BUILD RULES 

# Default target: build the ISO image
# "all" is what runs when you just type "make" with no arguments
# It depends on $(ISO), so Make will build the ISO (and everything it needs)
all: $(ISO)

# Rule to build the ISO image
# grub-mkrescue creates a bootable ISO with GRUB embedded in it
# Steps:
#   1. Create the directory structure GRUB expects inside the ISO
#   2. Copy our kernel binary to /boot/ in the ISO
#   3. Copy our grub.cfg to /boot/grub/ in the ISO
#   4. Run grub-mkrescue to package it all into a bootable .iso file
#
# mkdir -p: create directories (and parents), no error if they exist
# cp: copy files into the ISO directory structure
# grub-mkrescue: GRUB's tool that creates a bootable ISO image
#   -o specifies the output filename
$(ISO): $(KERNEL)
	mkdir -p $(ISO_DIR)/boot/grub
	cp $(KERNEL) $(ISO_DIR)/boot/tupleos.bin
	cp boot/grub.cfg $(ISO_DIR)/boot/grub/grub.cfg
	grub-mkrescue -o $(ISO) $(ISO_DIR)

# Rule to link all object files into the final kernel binary
# This is where the linker script (linker.ld) comes into play
# The linker combines boot.o and kernel.o, arranging sections
# according to linker.ld, producing the final ELF binary
#
# $@ = the target ($(KERNEL) = build/tupleos.bin)
# $^ = all prerequisites ($(OBJS) = build/boot.o build/kernel.o)
$(KERNEL): $(OBJS)
	mkdir -p $(BUILD_DIR)
	$(LD) $(LDFLAGS) -o $@ $^

# Rule to compile assembly files (.asm -> .o)
# Pattern rule: % is a wildcard that matches any filename
# So build/boot.o depends on boot/boot.asm
#
# $< = the first prerequisite (the .asm source file)
# $@ = the target (the .o output file)
$(BUILD_DIR)/boot.o: boot/boot.asm
	mkdir -p $(BUILD_DIR)
	$(AS) $< -o $@

# Rule to compile C files (.c -> .o)
# -c means "compile only, don't link" (we link separately above)
#
# $< = the first prerequisite (the .c source file)
# $@ = the target (the .o output file)
$(BUILD_DIR)/kernel.o: kernel/kernel.c
	mkdir -p $(BUILD_DIR)
	$(CC) $(CFLAGS) -c $< -o $@

# Run the OS in QEMU (a PC emulator)
# -cdrom: boot from our ISO as if it were a CD-ROM drive
# This is how you test without real hardware!
run: $(ISO)
	qemu-system-i386 -cdrom $(ISO)

# Clean up all generated files
# rm -rf: force remove recursively
# Deletes the build directory and ISO directory so you can start fresh
# .PHONY means "clean" isn't a file, it's a command
# (without this, if a file called "clean" existed, Make would get confused)
.PHONY: all clean run
clean:
	rm -rf $(BUILD_DIR) $(ISO_DIR)