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

                                      "redshift"
                                      "fontmanager"
                                      "xdg-utils"      ;; For xdg-open, etc
                                      "xdg-dbus-proxy" ;; For Flatpak
                                      "shared-mime-info"

                                      "icecat"

                                      "vlc"

                                      "mpv"
                                      "youtube-dl"
                                      "playerctl"

                                      "gimp"

                                      "flatpak"

                                      "pandoc"

                                      "emacs-next-pgtk-latest"
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
          (zshrc
           (list (local-file "./config/.zshrc" "zshrc")
                 ))))
        (simple-service 'some-useful-env-vars-service
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
