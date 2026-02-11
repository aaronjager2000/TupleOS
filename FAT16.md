How FAT16 Works
A FAT16 disk is laid out like this:


[ Boot Sector (BPB) ] [ Reserved Sectors ] [ FAT Table 1 ] [ FAT Table 2 ] [ Root Directory ] [ Data Region ]
 sector 0               sectors 1..N        N..M             M..P            P..Q              Q..end
The Boot Sector (sector 0 of the partition) contains the BIOS Parameter Block (BPB) — a struct at fixed byte offsets that tells you everything about the filesystem geometry:

Offset	Size	Field	What it means
0x0B	2	bytes_per_sector	Almost always 512
0x0D	1	sectors_per_cluster	Power of 2 (1, 2, 4, 8...)
0x0E	2	reserved_sectors	Sectors before FAT (includes boot sector)
0x10	1	num_fats	Usually 2 (redundant copies)
0x11	2	root_entry_count	Max entries in root dir (typically 512)
0x13	2	total_sectors_16	Total sectors on volume
0x16	2	fat_size_16	Sectors per FAT table
From these you derive everything:

fat_start = reserved_sectors
root_dir_start = reserved_sectors + (num_fats * fat_size_16)
root_dir_sectors = (root_entry_count * 32) / 512
data_start = root_dir_start + root_dir_sectors
cluster_to_sector(N) = data_start + (N - 2) * sectors_per_cluster
Clusters are numbered starting at 2 (0 and 1 are reserved in FAT).

The Root Directory is a flat array of 32-byte entries:

Offset	Size	Field
0x00	8	filename (space-padded)
0x08	3	extension (space-padded)
0x1A	2	first cluster number
0x1C	4	file size in bytes
So TUPLEOS.BIN is stored as "TUPLEOS " + "BIN".

The FAT table is an array of 16-bit entries. Entry N tells you what cluster comes after cluster N. Values 0xFFF8–0xFFFF mean "end of chain."

The read loop: read cluster → check FAT for next cluster → read that → repeat until end-of-chain.