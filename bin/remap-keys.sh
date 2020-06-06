#!/bin/sh

# Remap keys and set useful X preferences.
# Physical CapsLock becomes Left Control.
# Physical Left Control becomes Hyper.
# Physical WinKey becomes Super.

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

# Set the keyboard repeat to a high rate.
xset r rate 250 60
# Disable mouse acceleration (better experience with gaming mice).
xset m 1 1
fi
