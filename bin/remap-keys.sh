#!/bin/sh
# See http://www-2.cs.cmu.edu/~pach/ctrl-caps-win
# It's not clear what the Mod4 thing does.
#
# The windows key used to be 115 but is now 133 in xorg
# using evdev in Linux.
#

# /usr/bin/touch /tmp/remap-5
# 227 was attempt at Thinkpad Fn key
#    -e 'keycode 227 = XF86Start' \
#    -e 'keycode 23 = Hyper_L' \

# Original defaults:
# keycode 66 is CapsLock
# keycode 37 is Left Control
# keycode 133 is Win Key
# keycode 151 is Fn

# might need clear Lock
# add Mod3 = Super_L


# xmodmap -e 'clear Control' \
# 	-e 'keycode 66 = Control_L' \
# 	-e 'keycode 37 = Hyper_L' \
# 	-e 'keycode 133 = Caps_Lock' \
# 	-e 'add Control = Control_L' \
# 	-e 'add Control = Control_R'

# xmodmap -e "add mod3 = Super_L" \
# 	-e "add mod4 = Hyper_L"

# xmodmap -e 'keycode 37 = Hyper_L'

if [ $DISPLAY ]; then
 xmodmap \
     -e 'remove Lock = Caps_Lock' \
     -e 'remove Control = Control_L' \
     -e 'keycode 66 = Control_L' \
     -e 'add Control = Control_L'

xmodmap \
    -e 'clear mod3' \
    -e 'clear mod4' \
    -e 'clear Control' \
    -e 'keycode 66 = Control_L' \
    -e 'keycode 37 = Hyper_L' \
    -e 'add Control = Control_L' \
    -e 'add Control = Control_R' \
    -e 'add mod3 = Super_L' \
    -e 'add mod4 = Hyper_L'

xset r rate 250 60
fi
