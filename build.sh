#!/bin/bash
set -e

echo "=== TupleOS MyBoot Build ==="

# 1. Assemble stage 1 and stage 2
echo "[1/8] Assembling stage1..."
nasm -f bin boot/stage1.asm -o stage1.bin

echo "[2/8] Assembling stage2..."
nasm -f bin boot/stage2.asm -o stage2.bin

# 2. Build the kernel ELF binary using the existing Makefile
echo "[3/8] Building kernel..."
make -s build/tupleos.bin
if [ ! -f build/tupleos.bin ]; then
    echo "ERROR: Kernel build failed, build/tupleos.bin not found"
    exit 1
fi
KERNEL_SIZE=$(stat -c %s build/tupleos.bin 2>/dev/null || stat -f %z build/tupleos.bin)
echo "  Kernel size: $KERNEL_SIZE bytes"
if [ "$KERNEL_SIZE" -gt 589824 ]; then
    echo "ERROR: Kernel too large ($KERNEL_SIZE bytes > 576KB max)"
    exit 1
fi

# 3. Create 16MB blank disk image (32768 sectors x 512 bytes)
#    FAT16 requires at least ~4085 clusters, so we need a bigger image
#    than the old 1.44MB floppy. 16MB is comfortable.
echo "[4/8] Creating 16MB disk image..."
dd if=/dev/zero of=myboot.img bs=512 count=32768 status=none

# 4. Format as FAT16
#    -F 16  = force FAT16 (not FAT12 or FAT32)
#    -R 33  = 33 reserved sectors (1 boot sector + 32 for stage2)
#    -n     = volume label
#    mkfs.fat writes a valid BPB into sector 0 with correct geometry
echo "[5/8] Formatting as FAT16 (33 reserved sectors)..."
mkfs.fat -F 16 -R 33 -n "TUPLEOS" myboot.img

# 5. Inject stage1 boot code into sector 0, PRESERVING the BPB
#    mkfs.fat wrote correct BPB values at bytes 3-61 of sector 0.
#    We overwrite only our code bytes around it:
#      bytes 0-2:    our jmp short + nop
#      bytes 62-509: our boot code + data + padding + 0xAA55
echo "[6/8] Injecting stage1 into boot sector..."
dd if=stage1.bin of=myboot.img bs=1 count=3 conv=notrunc status=none
dd if=stage1.bin of=myboot.img bs=1 skip=62 seek=62 count=448 conv=notrunc status=none

# 6. Write stage2 into sectors 1-32 (the reserved sector area)
#    These sectors are marked "reserved" by the BPB, so the filesystem
#    won't touch them. Stage1 loads them into 0x7E00 at boot.
echo "[7/8] Writing stage2 to reserved sectors 1-32..."
dd if=stage2.bin of=myboot.img bs=512 seek=1 count=32 conv=notrunc status=none

# 7. Copy the real kernel ELF binary onto the FAT16 filesystem
#    mtools reads/writes FAT filesystems without needing mount.
echo "[8/8] Copying kernel ELF to filesystem..."
mcopy -i myboot.img build/tupleos.bin ::/TUPLEOS.BIN -o

echo ""
echo "=== Build complete ==="
echo "Disk image: myboot.img (16MB, FAT16)"
echo ""

# Verify the file is on the filesystem
echo "FAT16 directory listing:"
mdir -i myboot.img ::/
echo ""

echo "Run with: qemu-system-i386 -hda myboot.img"
