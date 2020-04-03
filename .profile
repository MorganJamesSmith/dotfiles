#!/bin/sh
# Profile file. Runs on login.

export EDITOR="emacsclient"
export TERMINAL="st"
export BROWSER="surf"
export READER="zathura"

export XDG_CONFIG_HOME="$HOME"/.config
export XDG_CACHE_HOME="$HOME"/.cache
export XDG_DATA_HOME="$HOME"/.local/share

export PASSWORD_STORE_DIR="$XDG_DATA_HOME"/password-store

export GPG_TTY=$(tty)
export GNUPGHOME="$XDG_DATA_HOME"/gnupg

export ZDOTDIR="$HOME"/.config

export HISTFILE="$XDG_DATA_HOME"/shell/history

export INPUTRC="$XDG_CONFIG_HOME"/readline/inputrc

[ -f "$HOME"/.config/hardwareprofile ] && . "$HOME"/.config/hardwareprofile

[ -f ~/.config/aliasrc ] && . "$HOME"/.config/aliasrc >/dev/null 2>&1

echo "$0" | grep "bash$" >/dev/null && [ -f "$HOME"/.bashrc ] && . "$HOME"/.bashrc
