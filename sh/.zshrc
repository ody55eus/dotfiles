#!/usr/bin/env zsh
# zsh config dir
export ZDOTDIR=$HOME/.config/zsh

# XDG Paths
export XDG_CONFIG_HOME=$HOME/.config
export XDG_CACHE_HOME=$HOME/.cache
export XDG_DATA_HOME=$HOME/.local/share

export GUIX_PROFILE=$HOME/.guix-profile
export GUIX_EXTRA=$HOME/.guix-extra
export GUIX_EXTRA_PROFILES=$HOME/.guix-extra-profiles
export GUIX_LOCPATH=${GUIX_PROFILE}/lib/locale

source $ZDOTDIR/zshrc
