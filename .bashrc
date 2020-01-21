# /etc/skel/.bashrc
#
# This file is sourced by all *interactive* bash shells on startup,
# including some apparently interactive shells such as scp and rcp
# that can't tolerate any output.  So make sure this doesn't display
# anything or bad things will happen !


# Test for an interactive shell.  There is no need to set anything
# past this point for scp and rcp, and it's important to refrain from
# outputting anything in those cases.
if [[ $- != *i* ]] ; then
	# Shell is non-interactive.  Be done now!
	return
fi

# Put your fun stuff here.
PATH=$HOME/bin:$PATH

alias ls='ls --color=auto'
alias la='ls -a'
alias lf='ls -FA'
alias ll='ls -lArt'
alias l='ls -lArt'
alias xo=xdg-open
alias e='emacs -fg grey -bg black -fn 7x14'

GREEN="\[\033[1;32m\]"
RED="\[\033[1;31m\]"
BLUE="\[\033[1;34m\]"
NOCOLOR="\[\033[0m\]"
export PS1='\u@\h'$GREEN' $PWD'$NOCOLOR'\n>'
export CLICOLOR=
export LSCOLORS="ExFxCxDxBxegedabagacad"

export EDITOR=emacs
