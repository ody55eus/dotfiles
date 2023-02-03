#!/usr/bin/env zsh
setxkbmap
emacs --daemon
picom -b
emacsclient -c &
tmux -l new-session -s "Main" -n "Dev" $(which zsh)
