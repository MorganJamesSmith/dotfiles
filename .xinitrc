#!/bin/sh
# Disable access control for the current user.
xhost +SI:localuser:"$USER"

# Make Java applications aware this is a non-reparenting window manager.
export _JAVA_AWT_WM_NONREPARENTING=1

# Set default cursor.
xsetroot -cursor_name left_ptr

# no beeping please
xset -b

# Set keyboard repeat rate.
xset r rate 200 60

xset +fp $(dirname $(readlink -f ~/.guix-profile/share/fonts/truetype/fonts.dir))

# Automatically hide cursor
command -v unclutter && unclutter &

# A compositor (optional)
command -v picom && picom -b

# This lets ssh-agent know what screen I'm using
gpg-connect-agent "UPDATESTARTUPTTY" /bye

# Finally start Emacs
exec emacs --fullscreen -f server-start
