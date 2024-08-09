#!/bin/sh
# Time-stamp: <2024-08-09 Fri 10:54>
# Copyright (C) 2024 by Morgan Smith

# This script installs guix system when run from a guix system installation medium

# Stop on error
set -e

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

# swap file
btrfs subvolume create /mnt/swap
btrfs filesystem mkswapfile /mnt/swap/swapfile --uuid clear -s 10g
swapon /mnt/swap/swapfile

# boot partition
mkfs.fat -F32 $p1
mkdir /mnt/boot
mount $p1 /mnt/boot

# obtain needed info
swap_offset=$(btrfs inspect-internal map-swapfile -r /swap/swapfile)
p1_uuid=$(lsblk -no UUID $p1)
p2_uuid=$(cryptsetup luksUUID $p2)

# grab needed files
cd /mnt
wget https://big.oisd.nl/dnsmasq2
wget https://git.sr.ht/~morgansmith/dotfiles/blob/master/.config/guix/config.scm

# insert info into files
sed -i "s/USERNAME/$user_name/" config.scm
sed -i "s/HOSTNAME/$host_name/" config.scm
sed -i "s/SWAP_OFFSET/$swap_offset/" config.scm
sed -i "s/BOOT_UUID/$p1_uuid/" config.scm
sed -i "s/LINUX_UUID/$p2_uuid/" config.scm

herd start cow-store /mnt
guix system init config.scm /mnt
