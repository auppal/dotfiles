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
export HISTFILE=$HOME/.bash_history

source ~/dotfiles/copy_clipboard.sh

# Based on: https://stackoverflow.com/questions/994563/integrate-readlines-kill-ring-and-the-x11-clipboard
kill_clipboard() {
    CUTBUFFER=${READLINE_LINE:$READLINE_POINT}
    copy_to_clipboard
    READLINE_LINE=${READLINE_LINE:0:$READLINE_POINT}
}
bind -x '"\C-k": kill_clipboard'

yank_clipboard() {
    PASTED=$(xclip -o)
    READLINE_LINE="${READLINE_LINE:0:$READLINE_POINT}$PASTED${READLINE_LINE:$READLINE_POINT}"
    READLINE_POINT=$(($READLINE_POINT + ${#PASTED}))
}
#bind -x '"\C-y": yank_clipboard'


OSC52_PASTE="\e]52;c;?\a"

backward-transform-pasted-line() {
    osc_len=$((${#READLINE_LINE} - $old_len - 5))
    offset=$(($start + 5))
    osc_encoded=${READLINE_LINE:$offset:$osc_len}
    osc_decoded=$(echo -n $osc_encoded | base64 -d)

    READLINE_LINE=${READLINE_LINE:0:$start}$osc_decoded${READLINE_LINE:$(($offset + $osc_len))}
    READLINE_POINT=$(($start + ${#osc_decoded}))

    # Remove the key binding.
    bind -r "\C-g"
}

paste_osc52() {
    start=$READLINE_POINT
    old_len=${#READLINE_LINE}
    bind -x '"\C-g": backward-transform-pasted-line'
    printf $OSC52_PASTE
}
bind -x '"\C-y": paste_osc52'

source ~/dotfiles/editor.sh
