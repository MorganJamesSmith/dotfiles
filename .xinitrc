#!/bin/sh
exec unclutter -idle 1 -jitter 5 &
exec slstatus &
exec cat ~/.config/sxhkd/sxhkdrcgenerator | envsubst > ~/.config/sxhkd/sxhkdrc &&
exec sxhkd -c ~/.config/sxhkd/sxhkdrc &
exec dunst &
exec dwm
