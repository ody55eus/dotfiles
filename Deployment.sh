#!/usr/bin/env bash
set -euo pipefail

# UPDATE="sudo pacman -Syu"
# INSTALL="sudo pacman -S"

# Yay has more packages
UPDATE="yay -Syu"
INSTALL="yay -S"

#UPDATE="apt update && apt upgrade -y"
#INSTALL="apt install"

git submodule update --init sh/.config/zsh/ohmyzsh
git submodule update --init doom/.emacs.d
git submodule update --init vim/.vim/bundle/Vundle.vim

$UPDATE
$INSTALL git neovim wget tmux stow

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

doom/.emacs.d/bin/doom install
