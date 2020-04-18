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

source ~/dotfiles/editor.sh

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
	export PS1=$'\e]0;%m %~\a'$'%n@%m \033[1;31m'$OS_NAME$'\033[1;32m%d\033[0m\n>'

	# See: http://unix.stackexchange.com/questions/7380/force-title-on-gnu-screen
	preexec () {
	    echo -ne "\ek${USER}@${HOST} $PWD ${1%% *}\e\\"
	    if [ $DISPLAY ] && [ $DISPLAY != ":0.0" ]; then
		export DISPLAY=`cat ~/.display`
	    fi
	}
	;;
    xterm*)
        export PS1=$'\e]0;%m %~\a'$'%n@%m \033[1;31m'$OS_NAME$'\033[1;32m%d\033[0m\n>'
	;;
    eterm*)
        # emacs won't set window title
	export PS1=$'%n@%m \033[1;32m%d\033[0m\n>'
	;;
    rxvt*)
        export PS1=$'\e]0;%m %~\a'$'%n@%m \033[1;31m'$OS_NAME$'\033[1;32m%d\033[0m\n>'
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
# export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LC_TIME=C

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

export TERMINFO=$HOME/.terminfo

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

eval $(dircolors) >& /dev/null

kill-line() { zle .kill-line ; copy_to_clipboard; }
zle -N kill-line

copy-region-as-kill() { zle .copy-region-as-kill ; copy_to_clipboard}
zle -N copy-region-as-kill

kill-region() { zle .kill-region; copy_to_clipboard }
zle -N kill-region

if [ $DISPLAY ] && which xclip >& /dev/null; then
    yank() { LBUFFER=$LBUFFER$(xclip -o) }
    zle -N yank
else
    if [[ $TERM = "rxvt-unicode-256color" || ($TERM = "xterm" && $MLTERM == "") || $TERM = "screen" || $TERM = "screen-256color" ]]; then
	# echo "osc52_paste enabled"
	yank() { paste_osc52 }
	zle -N yank
    fi
fi

# Emacs frame-background-mode (and others) use this to determine whether the color
# scheme is light or dark. But this is not passed through ssh, so default to dark
# in that case.
if [ ! $COLORFGBG ]; then
    export COLORFGBG="default;default;0"
fi

# Load the clipboard copy functions.
source ~/dotfiles/copy_clipboard.sh

# Base64 data in the response was inserted from the starting offset plus 6 header characters
# up until the new cursor location.

backward-transform-pasted-line() {
    ENCODED=${BUFFER[$(($start + 6)),$CURSOR]}
    LBUFFER=${LBUFFER[1,$start]}
    printf "\e[0m\e[?25h" # Restore text and cursor
    LBUFFER=$LBUFFER$(echo -n $ENCODED | base64 -d)
    bindkey "^G" send-break
}
zle -N backward-transform-pasted-line


if  [ $TERM = 'screen' ]; then
    if  [ $TMUX ]; then
	# Paste under tmux copies to tmux's internal clipboard.
	OSC52_PASTE="\ePtmux;\e\e]52;c;?\a\e\\"
	yank() { paste_osc52_tmux }
	zle -N yank
    else
	OSC52_PASTE="\eP\e]52;c;?\a\e\\"
    fi
else
    OSC52_PASTE="\e]52;c;?\a"
fi

paste_osc52() {
    start=$CURSOR
    # Temporarily install a handler to process the response, triggered by the "\a" bell character.
    bindkey "^G" backward-transform-pasted-line
    # Try to camouflage the pasted data with black text and invisible attribute, set hidden cursor,
    # and finally send the OSC 52 query string.
    # Using stty to disable echo does not work within zle.
    printf "\e[8;30m\e[?25l"
    printf $OSC52_PASTE
    # The OSC 52 response will be in the same form: \e]52;c;<base64 data>\a
}

paste_osc52_tmux() {
    printf $OSC52_PASTE
    sleep 0.05
    LBUFFER=$LBUFFER$(tmux paste)
}
