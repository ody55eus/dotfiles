(use-modules (gnu home)
             (gnu home services)
             (gnu home services shells)
             (gnu services)
             (guix gexp))

(home-environment
 (packages (list
            
            (specifications->manifest
             '(
               
               "system-config-printer"
               
               "emacs-next-pgtk-latest"
               
               "direnv"
               "zsh"
               "tmux"
               "openssh"
               "git"
               "bat"
               "zip"
               "unzip"
               "ripgrep"
               "the-silver-searcher" ; ag
               "trash-cli"

               "glibc-locales"
               "nss-certs"
               
               ))
            
            ))
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
             ("ZSH" . "/home/jp/.config/zsh/ohmyzsh")
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
           (list (local-file "./config/.zshrc" "zshrc")
                 (let ((commit "d41ca84af1271e8bfbe26f581cebe3b86521d0db")
                   (origin
                    (method git-fetch)
                    (uri (git-reference
                          (url "https://github.com/ohmyzsh/ohmyzsh.git/")
                          (commit commit)))
                    (file-name "ohmyzsh")
                    (sha256
                     (base32
                      "0kj252ywhc0jw0j7mr3dwx4q5mi5rrlm4jlhc8mbx66ylfvxi9qg")))))
                 )))))))
