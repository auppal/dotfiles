#!/bin/sh
ipython profile create
echo "c.TerminalIPythonApp.interactive_shell_class = 'InteractiveCopyShell.InteractiveCopyShell'" >> ~/.ipython/profile_default/ipython_config.py
ln -s ~/dotfiles/copy-to-clipboard ~/.ipython/copy_to_clipboard.py
ln -s ~/dotfiles/InteractiveCopyShell.py ~/.ipython/
