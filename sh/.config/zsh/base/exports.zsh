#!/bin/sh
HISTFILE="$ZDOTDIR"/.zsh_history
HISTSIZE=1000000
SAVEHIST=500000

export MANWIDTH=999
export PATH="$HOME/.local/bin":$PATH
export PATH="$HOME/.bin":$PATH
export PATH="$HOME/.emacs.d/bin":$PATH

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='nvim'
  ### "nvim" as manpager
  export MANPAGER="nvim -c 'set ft=man' -"

  ### "less" as manpager
  # export MANPAGER='less -M'
else
  export EDITOR="emacsclient -t -a 'nvim'"                 # $EDITOR use Emacs in terminal
  export VISUAL="emacsclient -c -a emacs"              # $VISUAL use Emacs in GUI mode
  ### "nvim" as manpager
  export MANPAGER="nvim -c 'set ft=man' -"
  # Emacs to view man pages
  # export MANPAGER="emacsclient -nw -e \"(let ((Man-notify-method 'bully)) (man \\\"\$1\\\")\"'"
fi

# Make Python use UTF-8 encoding for output to stdin, stdout, and stderr.
export PYTHONIOENCODING='UTF-8';

# Prefer US English and use UTF-8.
export LANG='en_US.UTF-8';
export LC_ALL='en_US.UTF-8';

# Avoid issues with `gpg` as installed via Homebrew.
# https://stackoverflow.com/a/42265848/96656
export GPG_TTY=$(tty);

# Hide the “default interactive shell is now zsh” warning on macOS.
export BASH_SILENCE_DEPRECATION_WARNING=1;

# Speedy keys
# kbdrate -d 200 -r 30
# xset r rate 200 30


# eval "$(fnm env)"
# eval "$(zoxide init zsh)"
eval "`python -m pip completion --zsh`"
