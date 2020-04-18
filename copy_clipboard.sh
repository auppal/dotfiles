# Copy functions usable from zsh or bash.
#
copy_to_clipboard() {
    if  [ $DISPLAY ] && which xclip >& /dev/null; then
	printf "%s" $CUTBUFFER | xclip -i
    else
	copy_osc52
    fi
}

copy_osc52() {
    if  [[ $TERM = 'screen' || $TERM = 'screen-256color' ]]; then
	if  [ $TMUX ]; then
	    printf "\ePtmux;\e\e]52;c;$(printf '%s' $CUTBUFFER | base64 --wrap=0)\a\e\\"
	else
	    printf "\eP\e]52;c;$(printf '%s' $CUTBUFFER | base64 --wrap=0)\a\e\\"
	fi
    else
	printf "\e]52;c;%s\a" "$(printf "%s" $CUTBUFFER | base64 --wrap=0)"
    fi
}
