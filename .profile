#!/bin/sh
# Profile file. Runs on login.

export EDITOR="emacsclient"
export TERMINAL="st"
export BROWSER="surf"
export READER="zathura"

export XDG_CONFIG_HOME="$HOME"/.config
mkdir -p "$XDG_CONFIG_HOME"
export XDG_CACHE_HOME="$HOME"/.cache
mkdir -p "$XDG_CACHE_HOME"
export XDG_DATA_HOME="$HOME"/.local/share
mkdir -p "$XDG_DATA_HOME"

export PASSWORD_STORE_DIR="$XDG_DATA_HOME"/password-store
mkdir -p "$PASSWORD_STORE_DIR"

export GPG_TTY=$(tty)
export GNUPGHOME="$XDG_DATA_HOME"/gnupg
mkdir -p "$GNUPGHOME"

export INPUTRC="$XDG_CONFIG_HOME"/readline/inputrc
mkdir -p "$XDG_CONFIG_HOME"/readline

export HISTFILE="$XDG_DATA_HOME"/shell/history
mkdir -p "$XDG_DATA_HOME"/shell

# big history (bigger histories cause eshell to hang when starting)
export HISTFILESIZE=100000
export HISTSIZE=100000

# history time stamps
export HISTTIMEFORMAT="[%F %T] "
