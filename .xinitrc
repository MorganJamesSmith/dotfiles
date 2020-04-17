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

# Uncomment the following block to use the exwm-xim module.
#export XMODIFIERS=@im=exwm-xim
#export GTK_IM_MODULE=xim
#export QT_IM_MODULE=xim
#export CLUTTER_IM_MODULE=xim

# This lets ssh-agent know what screen I'm using
gpg-connect-agent "UPDATESTARTUPTTY" /bye

# Finally start Emacs
exec emacsclient -a "" -c
