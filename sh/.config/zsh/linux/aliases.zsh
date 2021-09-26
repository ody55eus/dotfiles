# Get updates for debian or arch respectively, update emacs, npm and gem
if [[ -f /etc/debian-release ]]; then
  alias update='sudo apt update && sudo apt upgrade -y && ~/.emacs.d/bin/doom upgrade; ~/.emacs.d/bin/doom build; npm install npm -g; npm update -g; sudo gem update --system; sudo gem update; sudo gem cleanup'
elif [[ -f /etc/arch-release ]]; then
  alias update='yay -Syu && ~/.emacs.d/bin/doom upgrade; ~/.emacs.d/bin/doom build;  npm install npm -g; npm update -g; sudo gem update --system; sudo gem update; sudo gem cleanup'
fi
