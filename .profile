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

export HISTFILE="$XDG_DATA_HOME"/shell/history

export INPUTRC="$XDG_CONFIG_HOME"/readline/inputrc



