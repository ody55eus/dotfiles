#!/bin/sh
HISTFILE="$ZDOTDIR"/.zsh_history
HISTSIZE=1000000
SAVEHIST=500000

export PATH="$HOME/.local/bin":$PATH
export MANWIDTH=999

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
  export MANPAGER='less -M'
else
  export EDITOR='nvim'
  export MANPAGER='vim +Man!'
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
xset r rate 350 40


# eval "$(fnm env)"
# eval "$(zoxide init zsh)"
eval "`pip completion --zsh`"
