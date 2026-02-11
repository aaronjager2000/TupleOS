#!/bin/bash
set -e

echo "=== TupleOS MyBoot Build ==="

# 1. Assemble stage 1 and stage 2
echo "[1/7] Assembling stage1..."
nasm -f bin boot/stage1.asm -o stage1.bin

echo "[2/7] Assembling stage2..."
nasm -f bin boot/stage2.asm -o stage2.bin

# 2. Create 16MB blank disk image (32768 sectors x 512 bytes)
#    FAT16 requires at least ~4085 clusters, so we need a bigger image
#    than the old 1.44MB floppy. 16MB is comfortable.
echo "[3/7] Creating 16MB disk image..."
dd if=/dev/zero of=myboot.img bs=512 count=32768 status=none

# 3. Format as FAT16
#    -F 16  = force FAT16 (not FAT12 or FAT32)
#    -R 33  = 33 reserved sectors (1 boot sector + 32 for stage2)
#    -n     = volume label
#    mkfs.fat writes a valid BPB into sector 0 with correct geometry
echo "[4/7] Formatting as FAT16 (33 reserved sectors)..."
mkfs.fat -F 16 -R 33 -n "TUPLEOS" myboot.img

# 4. Inject stage1 boot code into sector 0, PRESERVING the BPB
#    mkfs.fat wrote correct BPB values at bytes 3-61 of sector 0.
#    We overwrite only our code bytes around it:
#      bytes 0-2:    our jmp short + nop
#      bytes 62-509: our boot code + data + padding + 0xAA55
echo "[5/7] Injecting stage1 into boot sector..."
dd if=stage1.bin of=myboot.img bs=1 count=3 conv=notrunc status=none
dd if=stage1.bin of=myboot.img bs=1 skip=62 seek=62 count=448 conv=notrunc status=none

# 5. Write stage2 into sectors 1-32 (the reserved sector area)
#    These sectors are marked "reserved" by the BPB, so the filesystem
#    won't touch them. Stage1 loads them into 0x7E00 at boot.
echo "[6/7] Writing stage2 to reserved sectors 1-32..."
dd if=stage2.bin of=myboot.img bs=512 seek=1 count=32 conv=notrunc status=none

# 6. Copy a test kernel file onto the FAT16 filesystem
#    mtools reads/writes FAT filesystems without needing mount.
#    For now we use a dummy file -- stage2 just needs to find and load it.
#    Replace this with the real kernel binary when ready.
echo "[7/7] Copying test kernel to filesystem..."
printf 'TUPLEOS_KERNEL_PLACEHOLDER' > /tmp/TUPLEOS.BIN
mcopy -i myboot.img /tmp/TUPLEOS.BIN ::/TUPLEOS.BIN -o

echo ""
echo "=== Build complete ==="
echo "Disk image: myboot.img (16MB, FAT16)"
echo ""

# Verify the file is on the filesystem
echo "FAT16 directory listing:"
mdir -i myboot.img ::/
echo ""

echo "Run with: qemu-system-i386 -hda myboot.img"
