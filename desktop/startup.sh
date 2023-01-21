#!/usr/bin/env zsh
setxkbmap
emacs --daemon
picom -b
emacsclient -c &
tmux
