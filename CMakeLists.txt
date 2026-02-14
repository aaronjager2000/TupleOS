cmake_minimum_required(VERSION 3.20)

# Chatgpt converted CMakeLists.txt for clion support

# TupleOS - CMake build (cross i686-elf)
project(TupleOS C ASM)

# ------------------------------------------------------------
# Toolchain assumptions:
#  - You configure CMake with a toolchain file that sets:
#    CMAKE_C_COMPILER=i686-elf-gcc
#    CMAKE_ASM_COMPILER=i686-elf-as  (or i686-elf-gcc for asm)
#  - Example:
#    cmake -S . -B build-cmake -DCMAKE_TOOLCHAIN_FILE=cmake/i686-elf-toolchain.cmake
# ------------------------------------------------------------

set(BUILD_DIR ${CMAKE_BINARY_DIR})
set(ISO_DIR   ${CMAKE_BINARY_DIR}/iso)
set(KERNEL_BIN ${BUILD_DIR}/tupleos.bin)
set(ISO_IMAGE ${BUILD_DIR}/tupleos.iso)

# Match your Makefile flags
add_compile_options(
        -ffreestanding
        -O2
        -Wall
        -Wextra
        -nostdlib
        -fno-builtin
        -fno-stack-protector
)

include_directories(${CMAKE_SOURCE_DIR}/kernel)

# Linker flags from Makefile
set(LINKER_SCRIPT ${CMAKE_SOURCE_DIR}/linker.ld)
set(KERNEL_LINK_FLAGS
        "-nostdlib -T ${LINKER_SCRIPT} -lgcc -Wl,-z,max-page-size=0x1000"
)

# ------------------------------------------------------------
# Sources (same order intent as Makefile: boot first)
# Note: We assemble .asm using a custom command so we can use i686-elf-as.
# ------------------------------------------------------------

# Helper to assemble .asm -> .o
function(add_asm_object out_obj asm_file)
    add_custom_command(
            OUTPUT ${out_obj}
            COMMAND ${CMAKE_COMMAND} -E make_directory ${BUILD_DIR}
            COMMAND i686-elf-as ${asm_file} -o ${out_obj}
            DEPENDS ${asm_file}
            VERBATIM
    )
endfunction()

# Assemble objects (ensure boot.o first)
set(BOOT_O            ${BUILD_DIR}/boot.o)
set(GDT_FLUSH_O       ${BUILD_DIR}/gdt_flush.o)
set(INTERRUPTS_O      ${BUILD_DIR}/interrupts.o)
set(CONTEXT_SWITCH_O  ${BUILD_DIR}/context_switch.o)

add_asm_object(${BOOT_O}           ${CMAKE_SOURCE_DIR}/boot/boot.asm)
add_asm_object(${GDT_FLUSH_O}      ${CMAKE_SOURCE_DIR}/boot/gdt_flush.asm)
add_asm_object(${INTERRUPTS_O}     ${CMAKE_SOURCE_DIR}/boot/interrupts.asm)
add_asm_object(${CONTEXT_SWITCH_O} ${CMAKE_SOURCE_DIR}/boot/context_switch.asm)

add_custom_target(asm_objs DEPENDS
        ${BOOT_O}
        ${GDT_FLUSH_O}
        ${INTERRUPTS_O}
        ${CONTEXT_SWITCH_O}
)

# C sources
set(C_SOURCES
        ${CMAKE_SOURCE_DIR}/kernel/kernel.c
        ${CMAKE_SOURCE_DIR}/kernel/gdt.c
        ${CMAKE_SOURCE_DIR}/kernel/idt.c
        ${CMAKE_SOURCE_DIR}/kernel/keyboard.c
        ${CMAKE_SOURCE_DIR}/kernel/timer.c
        ${CMAKE_SOURCE_DIR}/kernel/shell.c
        ${CMAKE_SOURCE_DIR}/kernel/kprintf.c
        ${CMAKE_SOURCE_DIR}/kernel/serial.c
        ${CMAKE_SOURCE_DIR}/kernel/pmm.c
        ${CMAKE_SOURCE_DIR}/kernel/paging.c
        ${CMAKE_SOURCE_DIR}/kernel/kheap.c
        ${CMAKE_SOURCE_DIR}/kernel/vmm.c
        ${CMAKE_SOURCE_DIR}/kernel/process.c
        ${CMAKE_SOURCE_DIR}/kernel/scheduler.c
)

# Build C objects via an object library
add_library(kernel_c_objs OBJECT ${C_SOURCES})

# Ensure asm objects exist before linking
add_dependencies(kernel_c_objs asm_objs)

# Final kernel binary (we link the .o files explicitly to preserve boot-first intent)
add_executable(tupleos_kernel
        ${BOOT_O}
        ${GDT_FLUSH_O}
        ${INTERRUPTS_O}
        ${CONTEXT_SWITCH_O}
        $<TARGET_OBJECTS:kernel_c_objs>
)

# Output name/location to match Makefile: build/tupleos.bin
set_target_properties(tupleos_kernel PROPERTIES
        OUTPUT_NAME "tupleos"
        SUFFIX ".bin"
        RUNTIME_OUTPUT_DIRECTORY ${BUILD_DIR}
)

# Apply linker flags
set_target_properties(tupleos_kernel PROPERTIES
        LINK_FLAGS "${KERNEL_LINK_FLAGS}"
)

# Convenience variable for the produced file
add_custom_command(
        TARGET tupleos_kernel POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E echo "Kernel: ${KERNEL_BIN}"
)

# ------------------------------------------------------------
# ISO creation (grub-mkrescue)
# ------------------------------------------------------------
add_custom_target(iso ALL
        DEPENDS tupleos_kernel
        COMMAND ${CMAKE_COMMAND} -E make_directory ${ISO_DIR}/boot/grub
        COMMAND ${CMAKE_COMMAND} -E copy ${KERNEL_BIN} ${ISO_DIR}/boot/tupleos.bin
        COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_SOURCE_DIR}/boot/grub.cfg ${ISO_DIR}/boot/grub/grub.cfg
        COMMAND grub-mkrescue -o ${ISO_IMAGE} ${ISO_DIR}
        BYPRODUCTS ${ISO_IMAGE}
        VERBATIM
)

# ------------------------------------------------------------
# Run in QEMU (like make run)
# ------------------------------------------------------------
add_custom_target(run
        DEPENDS iso
        COMMAND qemu-system-i386 -cdrom ${ISO_IMAGE} -serial stdio
        VERBATIM
)

# ------------------------------------------------------------
# Clean helper (optional; normal "cmake --build . --target clean" works)
# ------------------------------------------------------------
add_custom_target(distclean
        COMMAND ${CMAKE_COMMAND} -E rm -rf ${BUILD_DIR}/build ${ISO_DIR} ${KERNEL_BIN} ${ISO_IMAGE}
        VERBATIM
)
