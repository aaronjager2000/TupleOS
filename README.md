# TupleOS

A hobby x86 operating system myself and val petrov building from scratch.

## Features

- VGA text mode terminal
- GDT/IDT setup
- Keyboard and timer drivers
- Basic shell
- Physical memory manager
- Paging
- Kernel heap

## Building

Requires an i686-elf cross-compiler toolchain.

```
make        # build the ISO
make run    # run in QEMU
make clean  # clean build files
```
