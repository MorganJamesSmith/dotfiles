#!/bin/bash
#  _               _
# | |__   __ _ ___| |__  _ __ ___
# | '_ \ / _` / __| '_ \| '__/ __|
# | |_) | (_| \__ \ | | | | | (__
# |_.__/ \__,_|___/_| |_|_|  \___|

HISTSIZE=HISTFILESIZE= # Infinite history.
PS1="[$USER]\$"
export PS1=$PS1



[ -f ~/.fzf.bash ] && source ~/.fzf.bash

# Use zsh if we're in an interactive shell and it's installed
[[ $- == *i* ]] && [[ -n "$(command -v zsh)" ]] && exec zsh
