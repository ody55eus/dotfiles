(use-modules (gnu home)
             (gnu home services)
             (gnu home services shells)
             (gnu services)
             (gnu packages)
             (gnu packages shells)
             (guix packages)
             (guix git-download)
             (dinos packages awesome-xyz)
             (dinos packages emacs-xyz)
             (dinos packages nvim)
             (dinos packages tmux)
             (dinos packages zsh-xyz)
             (guix gexp))

(home-environment
 (packages (specifications->packages (list
                                      ;; Terminal
                                      "zsh"
                                      "zsh-syntax-highlighting"
                                      "zsh-autopair"
                                      "zsh-autosuggestions"
                                      "tmux"
                                      "direnv"
                                      "gwl"
                                      "git"
                                      "git-flow"
                                      "bat"
                                      "fzf"
                                      "gnupg"
                                      "openssh"
                                      "glibc-locales"

                                      ;; Editors
                                      "neovim"
                                      "python-pynvim"

                                      ;; X-Tools
                                      "picom"
                                      "redshift"
                                      "brightnessctl"
                                      "xdg-utils"      ;; For xdg-open, etc
                                      "xdg-dbus-proxy" ;; For Flatpak
                                      "gtk+:bin"       ;; For gtk-launch
                                      "glib:bin"       ;; For gio-launch-desktop
                                      "shared-mime-info"
                                      "xset"
                                      "xrandr"         ; Screen-Resolution
                                      "xsel"           ; Manipulate Selections
                                      "dmenu"          ; Menu Launcher
                                      "rofi"           ; Application Launcher
                                      "pinentry"       ; X11-Password Entry

                                      "alacritty-next"
                                      "icecat"
                                      "vlc"
                                      "mpv"
                                      "youtube-dl"
                                      "playerctl"
                                      "gimp"
                                      "gucharmap"
                                      "fontmanager"

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
                                      "scrot"          ; CLI Screenshots
                                      "ghostscript"

                                      ;; Themes
                                      "adwaita-icon-theme"
                                      "hicolor-icon-theme"

                                      ;; Fonts
                                      "font-juliamono"
                                      "font-jetbrains-mono"
                                      "font-font-awesome"
                                      "font-nerd-fonts"

                                      ;; E-Mail
                                      "isync"
                                      "mu"

                                      ;; Self Defined
                                      "dotfiles-ody55eus"
                                      "zsh-ohmyzsh"
                                      "zsh-completions"
                                      "awesome-copycats"
                                      "tmux-tpm"

                                      ;; Emacs
                                      "emacs-next-pgtk-latest"
                                      "emacs-doom"

                                      ;; NeoVim
                                      ;; "neovim-lunarvim"
                                      "neovim-config"
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
             ("path" . "echo -e \"${PATH//:/\\n}\"")
             ("ips" . "grep -o 'inet6\\? \\(addr:\\)\\?\\s\\?\\(\\(\\([0-9]\\+\\.\\)\\{3\\}[0-9]\\+\\)\\|[a-fA-F0-9:]\\+\\)' | awk '{ sub(/inet6? (addr:)? ?/, \\\"\\\"); print }'")
             ("ll" . "ls -l")))
          (bashrc
           (list (local-file "./config/.bashrc" "bashrc")))))
        (service
         home-zsh-service-type
         (home-zsh-configuration
          (zshrc
           (list (local-file "./config/.zshrc" "zshrc")
                 ))))
        (simple-service 'home-folder-service
                        home-files-service-type
                        (list `(".vimrc"
                                ,(local-file "config/.vimrc" "vimrc-home"))
                              `(".config/nvim/init.lua"
                                ,(local-file "config/nvim-init.lua" "nvim-init.lua"))
                              `(".config/nvim/lua/user"
                                ,(file-append neovim-config "/share/lua/user"))
                              `(".config/doom/templates"
                                ,(local-file "../../doom/.doom.d/templates" "doom-templates" #:recursive? #t))
                              `(".config/doom/packages.el"
                                ,(local-file "../../doom/.doom.d/packages.el" "packages.el"))
                              `(".config/doom/org-workflow.el"
                                ,(local-file "../../doom/.doom.d/org-workflow.el" "org-workflow.el"))
                              `(".config/doom/config.el"
                                ,(local-file "../../doom/.doom.d/config.el" "config.el"))
                              `(".config/doom/init.el"
                                ,(local-file "../../doom/.doom.d/init.el" "init.el"))
                              `(".vim/.vimrc"
                                ,(local-file "config/.vim/vimrc" "vimrc"))
                              `(".config/bat/config"
                                ,(local-file "config/bat.config"))
                              `(".config/git/config"
                                ,(local-file "config/git.config"))
                              `(".config/git/attributes"
                                ,(local-file "config/git.attr"))
                              `(".config/git/ignore"
                                ,(local-file "config/git.ignore"))
                              `(".config/alacritty/alacritty.yml"
                                ,(local-file "config/alacritty.yml"))
                              `(".config/picom/picom.conf"
                                ,(local-file "config/picom.conf"))
                              `(".config/awesome/rc.lua"
                                ,(local-file "config/awesome.rc.lua"))
                              `(".config/awesome/themes"
                                ,(file-append awesome-copycats "/share/awesome/copycats/themes"))
                              `(".config/awesome/lain"
                                ,(file-append awesome-lain "/share/awesome/lain"))
                              `(".config/awesome/freedesktop"
                                ,(file-append awesome-freedesktop "/share/awesome/freedesktop"))
                              `(".config/emacs"
                                ,(file-append emacs-doom "/share/doom"))
                              `(".bin/doom"
                                ,(file-append emacs-doom "/bin/doom"))
                              `(".config/zsh/.p10k.zsh"
                                ,(local-file "config/.p10k.zsh" "p10k.zsh"))
                              `(".config/zsh/ohmyzsh"
                                ,(file-append zsh-ohmyzsh "/share/ohmyzsh"))
                              `(".cache/zsh/ohmyzsh/custom/themes"
                                ,(file-append zsh-powerlevel "/share/zsh/plugins/p10k"))
                              `(".cache/zsh/ohmyzsh/custom/plugins/zsh-completions"
                                ,(file-append zsh-completions "/share/zsh/plugins/zsh-completion"))
                              `(".bin/zsh"
                                ,(file-append zsh "/bin/zsh"))
                              `(".tmux/plugins/tpm"
                                ,(file-append tmux-tpm "/share/tmux/plugins/tpm"))
                              `(".tmux/themes/jp.sh"
                                ,(local-file "config/tmux/jp.theme" "tmux-jp.theme"))
                              `(".tmux-powerlinerc"
                                ,(local-file "config/tmux/.tmux-powerlinerc" "tmux-powerlinerc"))
                              `(".tmux.conf"
                                ,(local-file "config/tmux/.tmux.conf" "tmux.conf"))))
        (simple-service 'environment-variables-service
                        home-environment-variables-service-type
                        `(("LESSHISTFILE" . "$XDG_CACHE_HOME/.lesshst")
                          ("EDITOR" . "emacsclient -t -a nvim")
                          ("VISUAL" . "emacsclient -c -a emacs")
                          ("MANPAGER" . "nvim -c 'Man!' -o -")
                          ("ZDOTDIR" . "$HOME/.config/zsh")
                          ("HISTFILE" . "$ZDOTDIR/.zsh_history")
                          ("HISTSIZE" . "1000000")
                          ("SAVEHIST" . "500000")
                          ("MANWIDTH" . "999")
                          ("GPG_TTY" . "$(tty)")
                          ("KEYTIMEOUT" . "1")
                          ("PYTHONENCODING" . "UTF-8")
                          ("LANG" . "en_US.UTF-8")
                          ("LC_ALL" . "en_US.UTF-8")
                          ("GUIX_LOCPATH" . "$HOME/.guix-home/profile/lib/locale")
                          ("GUIX_EXTENSIONS_PATH" . "$HOME/.guix-home/profile/share/guix/extensions")
                          ("DOOMLOCALDIR" . "$XDG_DATA_HOME/doom")
                          ("DOOMPROFILELOADPATH" . "$XDG_CACHE_HOME/doom-profiles")
                          ("DOOMPROFILELOADFILE" . "$XDG_CACHE_HOME/doom/profiles/load.el")
                          ("ZSH" . "$ZDOTDIR/ohmyzsh")
                          ("ZSH_CUSTOM" . "$HOME/.cache/zsh/ohmyzsh/custom")
                          ("PATH" . "$HOME/.bin:$PATH")
                          ("SHELL" . "$HOME/.bin/zsh"))))))
