#!/bin/sh

export EDITOR="emacsclient"
export TERMINAL="st"
export BROWSER="surf"
export READER="zathura"


# big history (bigger histories cause eshell to hang when starting)
export HISTFILESIZE=100000
export HISTSIZE=100000

# history time stamps
export HISTTIMEFORMAT="[%F %T] "

# configure audio
( sh "$HOME"/.local/bin/jack.sh & ) > /dev/null 2>&1

# no beeping please
setterm -blength 0
amixer set Beep 0% mute &> /dev/null

### XDG Section
#
# This is only here to keep stuff out of my home directory

## Setup XDG variables
export XDG_CONFIG_HOME="$HOME"/.config
mkdir -p "$XDG_CONFIG_HOME"
export XDG_CACHE_HOME="$HOME"/.cache
mkdir -p "$XDG_CACHE_HOME"
export XDG_DATA_HOME="$HOME"/.local/share
mkdir -p "$XDG_DATA_HOME"

## Xauthority/ICEauthority
export XAUTHORITY="$XDG_RUNTIME_DIR"/Xauthority
export ICEAUTHORITY="$XDG_RUNTIME_DIR"/ICEauthority

## less
export LESSKEY="$XDG_CONFIG_HOME"/less/lesskey
mkdir -p "$XDG_CONFIG_HOME"/less
export LESSHISTFILE="$XDG_CACHE_HOME"/less/history
mkdir -p "$XDG_CACHE_HOME"/less

## pass
export PASSWORD_STORE_DIR="$XDG_DATA_HOME"/password-store
mkdir -p "$PASSWORD_STORE_DIR"

## GnuPG
export GPG_TTY=$(tty)
export GNUPGHOME="$XDG_DATA_HOME"/gnupg
mkdir -p "$GNUPGHOME"

## readline
export INPUTRC="$XDG_CONFIG_HOME"/readline/inputrc
mkdir -p "$XDG_CONFIG_HOME"/readline

## aspell
export ASPELL_CONF="per-conf $XDG_CONFIG_HOME/aspell/aspell.conf; personal $XDG_CONFIG_HOME/aspell/en.pws; repl $XDG_CONFIG_HOME/aspell/en.prepl"
mkdir -p "$XDG_CONFIG_HOME"/aspell

## shell
export HISTFILE="$XDG_DATA_HOME"/shell/history
mkdir -p "$XDG_DATA_HOME"/shell

### XDG Section End

# make SSH use gpg-agent
eval $(ssh-agent -s -a $(gpgconf --list-dirs agent-ssh-socket))

# start gpg-agent with moved homedir
eval $(gpg-agent --homedir $GNUPGHOME --daemon -s)

