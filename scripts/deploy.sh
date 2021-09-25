#!/usr/bin/env bash
set -euo pipefail

# Yay has more packages
UPDATE="yay -Syu"
INSTALL="yay -S"

# Alternatively use pacman
# UPDATE="sudo pacman -Syu"
# INSTALL="sudo pacman -S"

$UPDATE
$INSTALL git neovim wget tmux stow

$INSTALL conky

$INSTALL rofi dmenu

# Download OhMyZsh from github
git submodule update --init sh/.config/zsh/ohmyzsh
# Install OhMyZsh! Theme
rm -rf sh/.config/zsh/ohmyzsh/custom/themes
git clone https://github.com/romkatv/powerlevel10k.git sh/.config/zsh/ohmyzsh/custom/themes

mkdir -p ~/build
git clone https://git.savannah.gnu.org/git/emacs.git ~/build/emacs
cd ~/build/emacs
./autogen.sh
./configure
make -j`$(nproc)`
sudo make install
cd -

# Download Doom from Git
git submodule update --init doom/.emacs.d
# Install Doom
doom/.emacs.d/bin/doom install
