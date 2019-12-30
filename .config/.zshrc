#!/bin/zsh
# Based on Luke's config for the Zoomer Shell

# Enable colors and change prompt:
autoload -U colors && colors
PS1='%B%F{magenta}[%F{blue}%n %F{white}%~%F{magenta}]%F{blue}%(1j.(%j).)%(?.%F{green}.%F{red})%# %b%f'
GPG_TTY=$(tty)

# History in cache directory:
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.cache/zsh/history
if [[ ! -f $HISTFILE ]]; then
    mkdir -p "$(dirname $HISTFILE)"
    touch $HISTFILE
fi

# Basic auto/tab complete:
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)		# Include hidden files.

# vi mode
bindkey -v
export KEYTIMEOUT=1

# Use vim keys in tab complete menu:
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -v '^?' backward-delete-char

# Change cursor shape for different vi modes.
function zle-keymap-select {
  if [[ ${KEYMAP} == vicmd ]] ||
     [[ $1 = 'block' ]]; then
    echo -ne '\e[1 q'
  elif [[ ${KEYMAP} == main ]] ||
       [[ ${KEYMAP} == viins ]] ||
       [[ ${KEYMAP} = '' ]] ||
       [[ $1 = 'beam' ]]; then
    echo -ne '\e[5 q'
  fi
}
zle -N zle-keymap-select
zle-line-init() {
    zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
    echo -ne "\e[5 q"
}
zle -N zle-line-init
echo -ne '\e[5 q' # Use beam shape cursor on startup.
preexec() { echo -ne '\e[5 q' ;} # Use beam shape cursor for each new prompt.

# Edit line in vim with ctrl-e:
autoload edit-command-line; zle -N edit-command-line
bindkey '^e' edit-command-line

# Load aliases and shortcuts if existent.
[ -f "$HOME/.config/shortcutrc" ] && source "$HOME/.config/shortcutrc"
[ -f "$HOME/.config/aliasrc" ] && source "$HOME/.config/aliasrc"
[ -f "$HOME/.config/computerrc" ] && source "$HOME/.config/computerrc"

if [[ "$INSIDE_EMACS" = 'vterm' ]]; then

    export EDITOR=emacsclient

    function vim(){
        printf "\e]51;Efind-file \"$@\"\e\\"
    }

    function chpwd() {
        printf "\e]51;A$(whoami)@$(hostname):$(pwd)\e\\";
    }

    alias clear='printf "\e]51;Evterm-clear-scrollback\e\\";tput clear'
fi

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# Load zsh-syntax-highlighting; should be last.
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh &> /dev/null || \
source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh &> /dev/null
