xrdb -merge $HOME/.Xresources
source $HOME/dotfiles/bin/remap-keys.sh

(which emacs >& /dev/null) && emacs --daemon &
(which urxvtd >& /dev/null) && urxvtd &
(which xscreensaver >& /dev/null) && xscreensaver &
(which unclutter >& /dev/null) && unclutter -idle 3 &
(which parcellite >& /dev/null) && parcellite &

# Create an initial terminal.
(which urxvtc >& /dev/null) && urxvtc

# Other tiling window managers:
# exec awesome
# exec i3
# exec wmii
# exec qtile

# Notion requires utf8 for certain window titles
# LC_ALL=en_US.UTF-8
notion

urxvt -bg darkred
exit
