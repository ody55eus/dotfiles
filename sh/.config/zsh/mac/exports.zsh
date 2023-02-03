#!/usr/bin/env zsh
eval $(/opt/homebrew/bin/brew shellenv)

# Add my own install path on MacOS
# export PATH=/opt/jp/bin:$PATH

# Prefer US English and use UTF-8.
export LANG='en_US.UTF-8';
export LC_ALL='en_US.UTF-8';

# Highlight section titles in manual pages.
# export LESS_TERMCAP_md="${yellow}";

# Make vi (neovim) the default manpage viewer
# export MANPAGER="/bin/sh -c \"col -b | vi -c 'set ft=man ts=8 nomod nolist nonu noma' -\""
# Don’t clear the screen after quitting a manual page.
# export MANPAGER='less -X';

# Avoid issues with `gpg` as installed via Homebrew.
# https://stackoverflow.com/a/42265848/96656
export GPG_TTY=$(tty);

# Hide the “default interactive shell is now zsh” warning on macOS.
export BASH_SILENCE_DEPRECATION_WARNING=1;

# MacPorts
export PATH=/opt/local/bin${PATH:+:}$PATH
export MANPATH=/opt/local/share/man:$MANPATH

# zsh_add_file '/opt/homebrew/Cellar/docker/20.10.8/share/zsh/site-functions/_docker'
