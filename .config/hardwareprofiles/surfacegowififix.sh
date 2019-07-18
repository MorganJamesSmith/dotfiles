#!/bin/sh

if [ ! -f "board.bin" ]; then
	echo "ERROR: No board.bin file"
	exit
fi

rm /lib/firmware/ath10k/QCA6174/hw2.1/board.bin
cp board.bin /lib/firmware/ath10k/QCA6174/hw2.1/board.bin
rm /lib/firmware/ath10k/QCA6174/hw3.0/board.bin
cp board.bin /lib/firmware/ath10k/QCA6174/hw3.0/board.bin
