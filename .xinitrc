#!/bin/sh
exec unclutter -idle 1 -jitter 5 &
exec slstatus &
exec remaps &
exec dunst &
exec dwm
