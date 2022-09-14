(use-modules (gnu home)
             (gnu home services)
             (gnu home services shells)
             (gnu services)
             (gnu packages)
             (gnu packages shells)
             (guix packages)
             (guix git-download)
             (guix gexp))

(home-environment
 (packages (specifications->packages (list
                                      "emacs-next-pgtk-latest"
                                      "neovim"
                                      "tmux"

                                      "mcron"
                                      "unzip"
                                      "fzf"
                                      "ripgrep"
                                      "bat"
                                      "git"
                                      "git-flow"
                                      "stow"
                                      "openssh"
                                      "font-juliamono"
                                      "font-jetbrains-mono"
                                      "font-overpass"
                                      "font-awesome"

                                      "redshift"
                                      "fontmanager"
                                      "xdg-utils"      ;; For xdg-open, etc
                                      "xdg-dbus-proxy" ;; For Flatpak
                                      "shared-mime-info"
                                      "xrandr"
                                      "aspell"
                                      "aspell-dict-en"
                                      "aspell-dict-de"

                                      "alacritty"
                                      "icecat"
                                      "vlc"
                                      "mpv"
                                      "youtube-dl"
                                      "playerctl"
                                      "gimp"

                                      "flatpak"
                                      "rofi"

                                      "pandoc"
                                      )))
 (services
  (list
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
                              `(".doom.d/packages.el"
                                ,(local-file "../../doom/.doom.d/packages.el" "packages.el"))
                              `(".doom.d/org-workflow.el"
                                ,(local-file "../../doom/.doom.d/org-workflow.el" "org-workflow.el"))
                              `(".doom.d/config.el"
                                ,(local-file "../../doom/.doom.d/config.el" "config.el"))
                              `(".doom.d/init.el"
                                ,(local-file "../../doom/.doom.d/init.el" "init.el"))
                              `(".vim/.vimrc"
                                ,(local-file "config/.vim/vimrc" "vimrc"))
                              `(".tmux.conf"
                                ,(local-file "config/.tmux.conf" "tmux.conf"))))
        (simple-service 'environment-variables-service
                        home-environment-variables-service-type
                        `(("LESSHISTFILE" . "$XDG_CACHE_HOME/.lesshst")
                          ("EDITOR" . "emacsclient -t -a nvim")
                          ("VISUAL" . "emacsclient -c -a emacs")
                          ("MANPAGER" . "nvim -c 'Man!' -o -")
                          ("ZDOTDIR" . "$HOME/.config/zsh")
                          ("ZSH" . "$HOME/.config/zsh/ohmyzsh")
                          ("HISTFILE" . "$ZDOTDIR/.zsh_history")
                          ("HISTSIZE" . "1000000")
                          ("SAVEHIST" . "500000")
                          ("MANWIDTH" . "999")
                          ("GPG_TTY" . "$(tty)")
                          ("KEYTIMEOUT" . "1")
                          ("PYTHONENCODING" . "UTF-8")
                          ("LANG" . "en_US.UTF-8")
                          ("LC_ALL" . "en_US.UTF-8")
                          ("SHELL" . ,(file-append zsh "/bin/zsh")))))))
