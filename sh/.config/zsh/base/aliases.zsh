alias zsh-update-plugins="find "$ZDOTDIR/plugins" -type d -exec test -e '{}/.git' ';' -print0 | xargs -I {} -0 git -C {} pull -q"

# Text Editor
DEF_VI='nvim'
DEF_EDITOR=DEF_VI
alias vi=$DEF_VI
alias vimrc="$DEF_VI ~/.vim/vimrc"
alias nvimrc="$DEF_VI ~/.vim/nvim/init.vim"


# Colorize grep output (good for log files)
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'

# confirm before overwriting something
alias cp="cp -i"
alias mv='mv -i'
alias rm='rm -i'

# Some coloring after clear
alias clear='clear && colorscript -r'

# Shortcuts
alias d="cd ~/Documents"
alias dl="cd ~/Downloads"
alias g="git"
alias gs="git status"

#continue download
alias wget="wget -c"

# List all files colorized in long format
alias l="ls -laF"
alias la='ls -lAF'
# List only directories
alias lsd="ls -lAF | grep --color=never '^d'"

# easier to read disk
alias df='df -h'                          # human-readable sizes
alias free='free -m'                      # show sizes in MB


##########
## Special Commands
##########
#
# get top process eating memory
alias psmem='ps aux | sort -nr -k 4 | head -5'

# get top process eating cpu ##
alias pscpu='ps aux | sort -nr -k 3 | head -5'

#get the error messages from journalctl
alias jctl="journalctl -p 3 -xb"

# gpg encryption
# verify signature for isos
alias gpg-check="gpg --keyserver-options auto-key-retrieve --verify"
# receive the key of a developer
alias gpg-retrieve="gpg --keyserver-options auto-key-retrieve --receive-keys"

# Displays the keybiding number of pressed keys.
# use with 
# $ xmodmap -e 'keycode 108 = 
AWK_CMD='/^KeyPress/ { a[NR+2] } NR in a { printf "%-3s %s\n", $5, $8 }'
alias show-keybindings="xev | awk -F'[ )]+' '"${AWK_CMD}"'"

alias m="git checkout main"
alias s="git checkout stable"

#quickly kill conkies
alias kc='killall conky'

#hardware info --short
alias hw="hwinfo --short"

#Recent Installed Packages
alias rip="expac --timefmt='%Y-%m-%d %T' '%l\t%n %v' | sort | tail -200 | nl"
alias riplong="expac --timefmt='%Y-%m-%d %T' '%l\t%n %v' | sort | tail -3000 | nl"

#youtube-dl
alias yta-aac="youtube-dl --extract-audio --audio-format aac "
alias yta-best="youtube-dl --extract-audio --audio-format best "
alias yta-flac="youtube-dl --extract-audio --audio-format flac "
alias yta-m4a="youtube-dl --extract-audio --audio-format m4a "
alias yta-mp3="youtube-dl --extract-audio --audio-format mp3 "
alias yta-opus="youtube-dl --extract-audio --audio-format opus "
alias yta-vorbis="youtube-dl --extract-audio --audio-format vorbis "
alias yta-wav="youtube-dl --extract-audio --audio-format wav "

alias ytv-best="youtube-dl -f bestvideo+bestaudio "

case "$(uname -s)" in

   Darwin)
     # echo 'Mac OS X'
	alias ls='ls -G'
     ;;

   Linux)
     ;;

   CYGWIN*|MINGW32*|MSYS*|MINGW*)
     # echo 'MS Windows'
     ;;
   *)
     # echo 'Other OS' 
     ;;
esac

# Enable aliases to be sudo’ed
alias sudo='sudo '

# Get week number
alias week='date +%V'

# Get macOS Software Updates, and update installed Ruby gems, Homebrew, npm, and their installed packages
alias update='sudo apt update && sudo apt upgrade -y'

# IP addresses
alias ip="dig +short myip.opendns.com @resolver1.opendns.com"
alias localip="ipconfig getifaddr en0"
alias ips="ifconfig -a | grep -o 'inet6\? \(addr:\)\?\s\?\(\(\([0-9]\+\.\)\{3\}[0-9]\+\)\|[a-fA-F0-9:]\+\)' | awk '{ sub(/inet6? (addr:)? ?/, \"\"); print }'"

# URL-encode strings
alias urlencode='python -c "import sys, urllib as ul; print ul.quote_plus(sys.argv[1]);"'

# Merge PDF files, preserving hyperlinks
# Usage: `mergepdf input{1,2,3}.pdf`
alias mergepdf='gs -q -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -sOutputFile=_merged.pdf'

# Print each PATH entry on a separate line
alias path='echo -e ${PATH//:/\\n}'
