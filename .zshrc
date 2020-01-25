# Written by Ahsen Uppal
# Copyright (C) 2017, Ahsen Uppal
# All rights reserved.
#

case $OSTYPE in
    linux*)
        alias ls='ls --color=auto'
	alias grep='grep --color=auto'
	export GCC_COLORS=auto	
	alias time='/usr/bin/time'
    ;;
    freebsd*)
        export CLICOLOR=
        export LSCOLORS="ExFxCxDxBxegedabagacad"
    ;;
esac

alias la='ls -a'
alias lf='ls -FA'
alias ll='ls -lArt'
alias l='ls -lArt'
alias xo=xdg-open


# if [ $TERM = "rxvt-unicode-256color" ]; then
#     alias ssh='TERM=xterm-color ssh'
# fi

if which eipe >& /dev/null; then
#    e() { eipe "$@" >& /dev/null || (if [ $DISPLAY ]; then (emacsclient -c --alternate-editor="" -q "$@" -n) else (emacsclient -c --alternate-editor="" -q "$@") fi) }
    # Disable xterm cell motion mouse tracking after exiting emacsclient.
    e() { eipe "$@" >& /dev/null || (emacsclient --tty -c --alternate-editor="" -q "$@"; printf '\e[?1002l') }
    export EDITOR='emacsclient --tty -c --alternate-editor="" -q '
elif which emacs >& /dev/null; then
    export EDITOR='emacsclient -c --alternate-editor="" -q '
    alias e=$EDITOR
else
    export EDITOR=mg
    alias e=$EDITOR
fi

if [ -f /etc/os-release ]; then
    source /etc/os-release
    if [ $PRETTY_NAME != "Gentoo/Linux" ]; then
	OS_NAME=$PRETTY_NAME' '
    else
	OS_NAME=
    fi
else
    OS_NAME=$(uname -o)' '
fi

# set the prompt to both change window title and color the prompt

case $TERM in
    cons25*)
	export PS1=$'%n@%m \033[1;32m%d\033[0m\n>'
	;;
    linux*)
	export PS1=$'%n@%m \033[1;32m%d\033[0m\n>'
	;;
    screen*)
	export PS1=$'\033]0;%n@%m %d\007'$'%n@%m \033[1;31m'$OS_NAME$'\033[1;32m%d\033[0m\n>'

	# See: http://unix.stackexchange.com/questions/7380/force-title-on-gnu-screen
	preexec () {
	    echo -ne "\ek${USER}@${HOST} $PWD ${1%% *}\e\\"
	    if [ $DISPLAY ] && [ $DISPLAY != ":0.0" ]; then
		export DISPLAY=`cat ~/.display`
	    fi
	}
	;;
    xterm*)
        export PS1=$'\033]0;%n@%m %d\007'$'%n@%m \033[1;31m'$OS_NAME$'\033[1;32m%d\033[0m\n>'
	;;
    eterm*)
        # emacs won't set window title
	export PS1=$'%n@%m \033[1;32m%d\033[0m\n>'
	;;
    rxvt*)
        export PS1=$'\033]0;%n@%m %d\007'$'%n@%m \033[1;31m'$OS_NAME$'\033[1;32m%d\033[0m\n>'
        ;;
    *)
	export PS1=$'%n@%m %d\n>'
	;;
esac

stty -ixon
autoload -U compinit
compinit

zstyle ':completion::complete:*' use-cache 1
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt $'\033[1;42m'$'\033[1;30m'%SAt %p: Hit TAB for more, or the character to insert%s$'\033[0m'
zstyle :compinstall filename $HOME/.zshrc

# Force a carrige return before the prompt.
unsetopt promptcr

export PATH=$HOME/bin:$PATH:/sbin

# Fix M-b and M-f of /, etc.
export WORDCHARS=''
export LC_ALL=en_US.UTF-8

#if [ -z "$STY" ]; then
#    screen -xRR
#fi

export HISTSIZE=500000
export HISTFILE=~/.zsh_history
# History won't be saved without this:
export SAVEHIST=$HISTSIZE

# With sharehistory enabled, no need to set append_history or inc_append_history
setopt sharehistory
setopt hist_ignore_space # No saving for cmds beginning with a space

# correction
# setopt correctall

kill-line() { zle .kill-line ; printf "\e]52;c;$(echo -n $CUTBUFFER | base64)\a" }
zle -N kill-line

copy-region-as-kill() { zle .copy-region-as-kill ; printf "\e]52;c;$(echo -n $CUTBUFFER | base64)\a" }
zle -N copy-region-as-kill

kill-region() { zle .kill-region; printf "\e]52;c;$(echo -n $CUTBUFFER | base64)\a" }
zle -N kill-region

if  [ $DISPLAY ] && which xclip >& /dev/null; then
    yank() { LBUFFER=$LBUFFER$(xclip -o) }
    zle -N yank
fi

bindkey -e
bindkey "^K" kill-line
bindkey "^W" kill-region
bindkey "^Y" yank
bindkey "^[h" backward-delete-word
zle_highlight+=(paste:none;region:bg=blue)

source ~/.zshrc_private >& /dev/null

# Store the time for every command run
# See: http://stackoverflow.com/questions/12580675/zsh-preexec-command-modification
    
function time_and_accept {
    BUFFER="/usr/bin/time -ao /tmp/time-stats $BUFFER"
    zle accept-line
}
zle -N time_and_accept_widget time_and_accept
# Uncomment to enable
# bindkey '^J' time_and_accept_widget
# bindkey '^M' time_and_accept_widget

export LESS='-XFR'

which eman >& /dev/null && alias man=eman && compdef _man eman

# From: http://boredzo.org/blog/archives/2016-08-15/colorized-man-pages-understood-and-customized
# and http://unix.stackexchange.com/questions/6010/colored-man-pages-not-working-on-gentoo
export GROFF_NO_SGR=1
export LESS_TERMCAP_mb=$'\e'"[1;31m"
export LESS_TERMCAP_md=$'\e'"[1;31m"
export LESS_TERMCAP_me=$'\e'"[0m"
export LESS_TERMCAP_se=$'\e'"[0m"
export LESS_TERMCAP_so=$'\e'"[1;44;33m"
export LESS_TERMCAP_ue=$'\e'"[0m"
export LESS_TERMCAP_us=$'\e'"[1;32m"

which dircolors >& /dev/null && eval $(dircolors)
