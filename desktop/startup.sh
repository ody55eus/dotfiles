#!/usr/bin/env zsh
sudo mount -t vboxsf share /home/jp/share
sudo dhclient
emacs --daemon
picom -b
tmux -l new-session -s "Main" -n "Dev" $(which zsh)
