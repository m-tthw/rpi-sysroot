## init_resize.sh mod (fork of rpi-sysroot)
This repository only contains a modified version of init_resize.sh.

The script will:

- extend the root partition to 8Gb
- create a mirror partition of root, unmounted by default
- create a data partition with the SD's remaining space
- unoumnt boot partition by defautl

To be used with a read-only overlay on the root partitions.
