;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(use-modules
  (gnu home)
  (gnu packages)
  (gnu services)
  (guix gexp)
  (gnu home services shells))

(home-environment
 (packages (specifications->packages (list
                                      "icecat"
                                      "nyxt"

                                      "alsa-utils"
                                      "pavucontrol"

                                      "vlc"

                                      "mpv"
                                      "youtube-dl"
                                      "playerctl"

                                      "gstreamer"
                                      "gst-plugins-base"
                                      "gst-plugins-good"
                                      "gst-plugins-bad"
                                      "gst-plugins-ugly"
                                      "gst-libav"
                                      "intel-vaapi-driver"
                                      "libva-utils"

                                      "feh"
                                      "gimp"
                                      "scrot"

                                      "zathura"
                                      "zathura-pdf-mupdf"

                                      "flatpak"

                                      "system-config-printer"

                                      "compton"
                                      "redshift"
                                      "gucharmap"
                                      "fontmanager"
                                      "brightnessctl"
                                      "xdg-utils"      ;; For xdg-open, etc
                                      "xdg-dbus-proxy" ;; For Flatpak
                                      "gtk+:bin"       ;; For gtk-launch
                                      "glib:bin"       ;; For gio-launch-desktop
                                      "shared-mime-info"

                                      "font-ibm-plex"
                                      "font-overpass"
                                      "font-jetbrains-mono"

                                      "texlive"
                                      "pandoc"

                                      "isync"
                                      "mu"

                                      "emacs-next-pgtk-latest"

                                      "openssh"
                                      "git"
                                      "bat"
                                      "zip"
                                      "unzip"
                                      "ripgrep"
                                      "the-silver-searcher" ; ag
                                      "trash-cli"
                                      "alacritty"
                                      "direnv"
                                      "zsh"
                                      "zsh-autosuggestions"
                                      "zsh-syntax-highlighting"
                                      "tmux"
                                      "glibc-locales"

                                      "xev"
                                      "xset"
                                      "xrdb"
                                      "xhost"
                                      "xmodmap"
                                      "setxkbmap"
                                      "xrandr"
                                      "arandr"
                                      "xss-lock"
                                      "libinput"
                                      "xinput"
                                      )))
  (services
    (list (service
            home-bash-service-type
            (home-bash-configuration
              (aliases
                '(("l" . "ls -CF")
                  ("la" . "ls -A")
                 ("vi" . "nvim")
                 ("wget" . "wget -c")
                 ("lsd" . "ls -lAF | grep --color=never '^d'")
                 ("df" . "df -h")
                 ("psmem" . "ps aux | sort -nr -k 4 | head -5")
                 ("pscpu" . "ps aux | sort -nr -k 3 | head -5")
                 ("gpg-check" . "gpg --keyserver-options auto-key-retrieve --verify")
                 ("gpg-retrieve" . "gpg --keyserver-options auto-key-retrieve --receive-keys")
                 ("mergepdf" . "gs -q -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -sOutputFile=_merged.pdf")
                 ("path" . "echo -e ${PATH//:/\\n}")
                 ("ips" . "grep -o 'inet6\\? \\(addr:\\)\\?\\s\\?\\(\\(\\([0-9]\\+\\.\\)\\{3\\}[0-9]\\+\\)\\|[a-fA-F0-9:]\\+\\)' | awk '{ sub(/inet6? (addr:)? ?/, \\\"\\\"); print }'")
                  ("ll" . "ls -l")))
              (bashrc
                (list (local-file "./config/.bashrc" "bashrc")))))
          (service
            home-zsh-service-type
            (home-zsh-configuration
              (environment-variables
               '(("ZDOTDIR" . "/home/jp/.config/zsh")
                 ("HISTFILE" . "$ZDOTDIR/.zsh_history")
                 ("HISTSIZE" . "1000000")
                 ("SAVEHIST" . "500000")
                 ("MANWIDTH" . "999")
                 ("EDITOR" . "\"emacsclient -t -a 'nvim'\"")
                 ("VISUAL" . "\"emacsclient -c -a 'emacs'\"")
                 ("MANPAGER" . "\"nvim -c 'Man!' -o -\"")
                 ("PYTHONENCODING" . "UTF-8")
                 ("LANG" . "en_US.UTF-8")
                 ("LC_ALL" . "en_US.UTF-8")
                 ("GPG_TTY" . "$(tty)")
                 ("KEYTIMEOUT" . "1")
                 ))
              (zshrc
                (list (local-file "./config/.zshrc" "zshrc"))))))))
