#!/bin/sh
# Time-stamp: <2025-09-04 Thu 13:30>
# Copyright (C) 2024 by Morgan Smith

# This script installs guix system when run from a guix system installation medium

# Stop on error
set -e

# Discover substitutes on local network
herd discover guix-daemon on

# Change these variables
disk=/dev/sda
p1=/dev/sda1
p2=/dev/sda2
host_name="HOSTNAME"
user_name="USERNAME"

# Create partitions
sfdisk -w always $disk << EOF
label: gpt
# EFI partition
size=500MiB,  type=uefi,
# Linux partition
type=linux,
EOF

# Print out partition info
fdisk -l $disk

# Encrypt main partition
cryptsetup luksFormat --type luks2 --pbkdf pbkdf2 $p2
cryptsetup open $p2 guix-root
cryptsetup --perf-no_read_workqueue --perf-no_write_workqueue --allow-discards --persistent refresh guix-root

# Put BTRFS on main partition and mount
mkfs.btrfs -L guix-root /dev/mapper/guix-root
mount -o compress=lzo,lazytime LABEL=guix-root /mnt

# Add a keyfile to avoid needing the type the password twice on boot
# TODO: test this.  Not a clue if it works
dd bs=512 count=4 if=/dev/random iflag=fullblock of=/mnt/key-file.bin
cryptsetup luksAddKey $p2 /mnt/key-file.bin
echo /mnt/key-file.bin | cpio -oH newc >/mnt/key-file.cpio
chmod 0000 /mnt/key-file.bin
chmod 0000 /mnt/key-file.cpio

# swap file
swap_location="/mnt/swap"
swap_file="$swap_location/swapfile"
btrfs subvolume create $swap_location
btrfs filesystem mkswapfile $swap_file --uuid clear -s 10g
swapon $swap_file

# boot partition
mkfs.fat -F32 $p1
mkdir /mnt/boot
mount $p1 /mnt/boot

# obtain needed info
swap_offset=$(btrfs inspect-internal map-swapfile -r $swap_file)
p1_uuid=$(lsblk -no UUID $p1)
p2_uuid=$(cryptsetup luksUUID $p2)

# grab needed files
cd /mnt
wget https://big.oisd.nl/dnsmasq2
wget https://git.sr.ht/~morgansmith/dotfiles/blob/master/.config/guix/config.scm
wget https://git.sr.ht/~morgansmith/dotfiles/blob/master/.config/guix/home.scm
wget https://git.sr.ht/~morgansmith/dotfiles/blob/master/.config/guix/default-manifest.scm
wget https://git.sr.ht/~morgansmith/dotfiles/blob/master/.config/guix/emacs-manifest.scm
wget https://git.sr.ht/~morgansmith/dotfiles/blob/master/.config/guix/transformations.scm

cat > machine-specific.scm <<EOF
(define username "$user_name")
(define host-name "$host_name")
(define swap-offset "$swap_offset")
(define linux-uuid "$p2_uuid")
(define boot-uuid "$p1_uuid")
EOF

herd start cow-store /mnt
guix system init config.scm /mnt
