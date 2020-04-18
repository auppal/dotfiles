
if which eipe >& /dev/null; then
    # Disable xterm cell motion mouse tracking after exiting emacsclient.
    editor() {
	printf '\e]0;'$@'\a\e]12;orange\a' # Window title and cursor color
	eipe "$@"
	printf '\e[?1002l\e]12;gray\a'
    }
    export EDITOR=eipe
    alias e=editor
elif which emacsclient >& /dev/null; then
    editor() {
	printf '\e]0;'$@'\a\e]12;orange\a' # Window title and cursor color
	emacsclient --tty -c --alternate-editor="" -q "$@"
	printf '\e[?1002l\e]12;gray\a'
	}
    export EDITOR='emacsclient -c --alternate-editor="" -q '
    alias e=editor
else
    export EDITOR=mg
    alias e=$EDITOR
fi

