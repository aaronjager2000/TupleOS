#TupleOS

A custom x86 operating system built from scratch in C and Assembly. This project aims to create a fully functional operating system with custom bootloader, complete hardware support, and modern OS features.

Current status: Phase 0 - Bare Bones Development
Target Architecture: x86 (32-bit initally, 64-bit upgrade planned)
Development team: 2 Developers, myself and val petrov (open source soon?)
Started: February 2026


#Project Specifications

Hardware Requirements:
CPU: x86-compatible processor (intel/AMD)
RAM: Minimum 512MB (for development/testing)
Storage: Any standard disk interface
Display: VGA-compatible display adapter
Input: PS/2 or USB keyboard/mouse

Software Architecture:
Kernel Type: Monolithic (initially), potential microkernel refactor
Boot Protocol: Multiboot (Phase 0-2), Custom bootloader (Phase 3+)
Executable Format: ELF
Filesystem: Custom FS (SimpleFS initially), FAT32/ext2 support later
User Interface: CLI (Phase 0-4), GUI (Phase 5+)

Technical Stack

Languages: C (kernel), x86 Assembly (low-level), C++ (future userspace)
Compiler: i686-elf-gcc cross-compiler
Assembler: NASM
Bootloader (current): GRUB2
Bootloader (future): Custom MyBoot
Build System: GNU Make
Testing: QEMU, Bochs, Real hardware


Development Roadmap
Phase 0: Bare Bones (Month 1-2) ✅ IN PROGRESS
Goal: Bootable kernel that prints to screen
Milestones:

 Development environment setup (WSL2, cross-compiler)
 Boot with GRUB
 VGA text mode driver
 Basic terminal output
 Keyboard input (scancode level)

Deliverables:

Bootable ISO that displays "Hello, kernel World!"
Basic text output functionality
Keyboard echo program


Phase 1: Core Infrastructure (Month 2-4)
Goal: Essential kernel infrastructure
Milestones:

 Proper GDT (Global Descriptor Table) setup
 IDT (Interrupt Descriptor Table) implementation
 PIC (Programmable Interrupt Controller) configuration
 Timer interrupt (PIT - Programmable Interval Timer)
 Keyboard interrupt handler (PS/2)
 Serial port driver (for debugging)
 Basic printf/printk implementation

Deliverables:

Hardware interrupt handling
Keyboard driver with proper key mapping
Debug output via serial port
System timer


Phase 2: Memory Management (Month 4-6)
Goal: Dynamic memory allocation and virtual memory
Milestones:

 Physical memory manager (bitmap/stack allocator)
 Page frame allocator
 Paging implementation (identity mapping initially)
 Virtual memory manager
 Kernel heap allocator (kmalloc/kfree)
 Higher-half kernel (kernel at 3GB+ virtual address)

Deliverables:

Working memory management subsystem
Dynamic memory allocation
Virtual address space per process
Memory protection


Phase 3: Custom Bootloader (Month 6-8) 
Goal: Replace GRUB with custom MyBoot bootloader
Milestones:

 Stage 1: MBR bootloader (512 bytes)
 Stage 2: Extended bootloader
 FAT12/16 filesystem support
 ELF loader
 32-bit protected mode switch
 Memory map detection (E820)
 A20 line enabling
 Multiboot-compatible kernel interface

Features:

MyBoot v1.0:

Boots from MBR/USB
Loads kernel by filename (no hardcoded sectors)
Simple configuration file
Basic menu system
Passes memory map to kernel
Minimal size (~50KB total)



Deliverables:

Custom bootloader replacing GRUB
Boot configuration system
Multi-boot menu support


Phase 4: Process Management (Month 8-10)
Goal: Multitasking operating system
Milestones:

 Process Control Block (PCB) structure
 Context switching
 Round-robin scheduler
 Process creation (fork/exec equivalent)
 Process termination
 Kernel threads
 User mode transition
 System call interface

Deliverables:

Preemptive multitasking
Multiple processes running concurrently
Basic syscalls (exit, fork, exec, wait)


Phase 5: Synchronization & IPC (Month 10-12)
Goal: Process communication and coordination
Milestones:

 Spinlocks
 Mutexes
 Semaphores
 Condition variables
 Message queues
 Shared memory
 Pipes (anonymous)
 Named pipes (FIFOs)
 UNIX signals

Deliverables:

Full IPC suite
Process synchronization primitives
Signal handling system


Phase 6: Storage & Filesystem (Month 12-15)
Goal: Persistent storage support
Milestones:

 ATA PIO driver (simple disk I/O)
 Virtual File System (VFS) layer
 Custom SimpleFS implementation
 FAT32 support (read/write)
 File descriptors
 Standard file operations (open, read, write, close)
 Directory operations
 File permissions

Deliverables:

Working filesystem
File I/O from userspace
Multiple filesystem support via VFS


Phase 7: Advanced Drivers (Month 15-18)
Goal: Expanded hardware support
Milestones:

 AHCI driver (SATA support)
 USB host controller (UHCI/EHCI)
 USB HID driver (keyboard/mouse)
 Network card driver (RTL8139 or Intel e1000)
 Basic TCP/IP stack (or port lwIP)
 Sound driver (AC97/Sound Blaster)
 RTC (Real-Time Clock)

Deliverables:

Modern hardware support
Network connectivity
USB device support


Phase 8: Shell & Utilities (Month 18-20)
Goal: Usable command-line environment
Milestones:

 Command shell (MyShell)
 Basic utilities (ls, cd, cat, cp, mv, rm, mkdir)
 Text editor (simple vi-like editor)
 Process management tools (ps, top, kill)
 File utilities (grep, find, wc)
 Scripting support (simple shell scripts)

Deliverables:

Interactive shell
UNIX-like utilities
Productive CLI environment


Phase 9: Multi-Core Support (Month 20-24)
Goal: SMP (Symmetric Multiprocessing)
Milestones:

 APIC (Advanced PIC) support
 CPU core detection
 AP (Application Processor) bootstrap
 Per-CPU data structures
 SMP-safe spinlocks
 CPU affinity
 Load balancing scheduler
 IPI (Inter-Processor Interrupts)

Deliverables:

Multi-core CPU utilization
Parallel task execution
SMP-safe kernel


Phase 10: User Space Development (Month 24-30)
Goal: Application development framework
Milestones:

 C standard library (libc) implementation
 Dynamic linker/loader
 Shared libraries (.so support)
 ELF dynamic linking
 POSIX-compatible APIs
 Development toolchain port (GCC/binutils)
 Self-hosting capability

Deliverables:

Full libc implementation
Ability to compile programs on MyOS
Self-hosting OS (can build itself)


Phase 11: Networking Stack (Month 30-36)
Goal: Full network capability
Milestones:

 Ethernet frame handling
 ARP protocol
 IP (Internet Protocol)
 ICMP (ping support)
 TCP implementation
 UDP implementation
 Socket API
 DNS resolver
 Basic network utilities (ping, netcat, wget)

Deliverables:

Complete TCP/IP stack
Network application support
Internet connectivity


Phase 12: Graphics & GUI (Month 36-48)
Goal: Graphical user interface
Milestones:

 Framebuffer driver
 VESA/VBE support
 Basic 2D graphics library
 Font rendering
 Window manager
 Widget toolkit
 Event system (mouse, keyboard)
 Compositor
 Desktop environment

Deliverables:

Graphical desktop environment
Window management
GUI applications framework


Phase 13: Advanced Features (Month 48+)
Goal: Modern OS capabilities
Milestones:

 64-bit (x86-64) upgrade
 UEFI boot support
 SSD optimization (TRIM support)
 Virtual memory optimization (COW, demand paging)
 Swap space
 Security features (user permissions, sandboxing)
 Power management (ACPI)
 Module system (loadable kernel modules)
 Package manager
 Virtualization support

Deliverables:

Modern OS feature parity
64-bit architecture support
Production-ready stability


Custom Bootloader Specification (MyBoot)
Design Philosophy

Minimal: Only what's necessary to boot our kernel
Fast: Quick boot times
Simple: Easy to understand and maintain
Flexible: Configuration file for boot options

MyBoot Architecture
Stage 1 (MBR - 446 bytes)
Location: First sector of boot device
Purpose: Initial bootstrap
Responsibilities:

Initialize basic CPU state
Load Stage 2 from known disk location
Transfer control to Stage 2

Code: Pure x86 assembly
Stage 2 (Extended Loader - ~50KB)
Location: Reserved sectors after MBR
Purpose: Main bootloader functionality
Responsibilities:

Initialize video mode (text or graphics)
Detect memory (E820 BIOS function)
Enable A20 line
Parse configuration file (myboot.cfg)
Display boot menu
Load kernel from filesystem
Parse ELF headers
Set up basic GDT
Switch to protected mode
Pass control to kernel with Multiboot-compatible info

Code: x86 assembly + C
MyBoot Features
Version 1.0 (MVP):

✅ Boot from MBR
✅ FAT16 filesystem support
✅ Load kernel by filename
✅ Simple text configuration
✅ Basic error messages
✅ Memory map detection
✅ Protected mode switch

Version 2.0 (Enhanced):

✅ Multiple filesystem support (FAT32, ext2)
✅ Boot menu with timeout
✅ Multiple kernel entries
✅ Command-line kernel parameters
✅ Graphical splash screen
✅ Module loading (initrd)

Version 3.0 (Advanced):

✅ UEFI support
✅ Secure boot
✅ Network booting (PXE)
✅ Chainloading other bootloaders
✅ Recovery mode
✅ Diagnostic tools


Future Capabilities (Complete OS Vision)
Core Features

✅ 32-bit and 64-bit support
✅ Preemptive multitasking
✅ Virtual memory management
✅ Multi-core CPU support (SMP)
✅ Modern filesystem support
✅ Network stack (TCP/IP)
✅ USB support
✅ Graphics and GUI

User Experience

✅ Command-line shell
✅ Graphical desktop environment
✅ Window manager
✅ Text editor
✅ File manager
✅ Web browser (basic)
✅ Terminal emulator
✅ System utilities

Development Environment

✅ Self-hosting (compile itself)
✅ GCC/binutils ported
✅ Native development tools
✅ C/C++ libraries
✅ Package manager
✅ Version control

Hardware Support

✅ Intel/AMD x86 CPUs
✅ SATA/NVMe storage
✅ USB 2.0/3.0
✅ Ethernet (multiple chipsets)
✅ WiFi (basic support)
✅ Sound cards
✅ Graphics cards (framebuffer + basic acceleration)

Networking

✅ Ethernet
✅ TCP/IP stack
✅ DNS resolution
✅ HTTP client
✅ SSH client/server
✅ FTP support
✅ Network utilities

Security

✅ User accounts and permissions
✅ Process isolation
✅ Memory protection
✅ Filesystem permissions
✅ Encrypted storage (future)
✅ Secure boot (future)