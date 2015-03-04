# Written by Ahsen Uppal
# Copyright (C) 2015, Ahsen Uppal
# All rights reserved.
#

case $OSTYPE in
    linux*)
        alias ls='ls --color=auto'
        alias e='emacs -fg grey -bg black -fn 7x14'
        which emacs-gnuclient-start >& /dev/null && alias e=emacs-gnuclient-start
    ;;
    freebsd*)
        export CLICOLOR=
        export LSCOLORS="ExFxCxDxBxegedabagacad"
        alias e='emacs -fg grey -bg black -fn 7x14'
    ;;
esac

alias la='ls -a'
alias lf='ls -FA'
alias ll='ls -lArt'
alias xo=xdg-open


# set the prompt to both change window title and color the prompt

case $TERM in
    cons25*)
	export PS1=$'[%n@%m] \033[1;32m%d\033[0m\n>'
	;;
    linux*)
	export PS1=$'[%n@%m] \033[1;32m%d\033[0m\n>'
	;;
    screen*)
	export PS1=$'\033]0;[%n@%m] %d\007'$'[%n@%m] \033[1;32m%d\033[0m\n>'

	# See: http://unix.stackexchange.com/questions/7380/force-title-on-gnu-screen
	preexec () {
	    echo -ne "\ek${USER}@${HOST} $PWD ${1%% *}\e\\"
	}
	;;
    xterm*)
	export PS1=$'\033]0;[%n@%m] %d\007'$'[%n@%m] \033[1;32m%d\033[0m\n>'
	;;
    eterm*)
        # emacs won't set window title
	export PS1=$'[%n@%m] \033[1;32m%d\033[0m\n>'
	;;
    rxvt*)
        export PS1=$'\033]0;[%n@%m] %d\007'$'[%n@%m] \033[1;32m%d\033[0m\n>'
        ;;
    kterm*)
        export PS1=$'\033]0;[%n@%m] %d\007'$'[%n@%m] \033[1;32m%d\033[0m\n>'
        ;;
    *)
	export PS1=$'[%n@%m] %d\n>'
	;;
esac

export EDITOR=emacs
export MANSECT="2:3:1:4:5:6:7:8:9"

stty -ixon
echo
which fortune >& /dev/null && fortune -o
echo

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
export LC_ALL=C

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

if which xclip >& /dev/null; then
kill-line() { zle .kill-line ; echo -n $CUTBUFFER | xclip -i }
zle -N kill-line
yank() { LBUFFER=$LBUFFER$(xclip -o) }
zle -N yank
bindkey "^K" kill-line
bindkey "^Y" yank
bindkey "^[h" backward-delete-word
fi

source .zshrc_private
