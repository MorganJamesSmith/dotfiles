#!/bin/sh
# Profile file. Runs on login.

(emacs --daemon &> /dev/null &)

# Adds `~/.bin` and all subdirectories to $PATH
export PATH="$PATH:$(du "$HOME/.bin/" | cut -f2 | tr '\n' ':' | sed 's/:*$//')"
# Adds `/opt/bin` to path
export PATH="$PATH:/opt/bin"
export EDITOR="nvim"
export TERMINAL="st"
export BROWSER="tabbed -c -r 2 surf -e ''"
export READER="zathura"

# less/man colors
export LESS=-R
export LESS_TERMCAP_mb="$(printf '%b' '[1;31m')"; a="${a%_}"
export LESS_TERMCAP_md="$(printf '%b' '[1;36m')"; a="${a%_}"
export LESS_TERMCAP_me="$(printf '%b' '[0m')"; a="${a%_}"
export LESS_TERMCAP_so="$(printf '%b' '[01;44;33m')"; a="${a%_}"
export LESS_TERMCAP_se="$(printf '%b' '[0m')"; a="${a%_}"
export LESS_TERMCAP_us="$(printf '%b' '[1;32m')"; a="${a%_}"
export LESS_TERMCAP_ue="$(printf '%b' '[0m')"; a="${a%_}"

export GPG_TTY=$(tty)
export XDG_CONFIG_HOME="$HOME/.config"
export ZDOTDIR="$HOME/.config"

[ -f $HOME/.config/hardwareprofile ] && source "$HOME/.config/hardwareprofile"

[ -f ~/.config/aliasrc ] && source "$HOME/.config/aliasrc" >/dev/null 2>&1

echo "$0" | grep "bash$" >/dev/null && [ -f $HOME/.bashrc ] && source "$HOME/.bashrc"

