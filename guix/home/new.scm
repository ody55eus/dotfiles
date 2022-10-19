(use-modules (gnu home)
             (gnu home services)
             (gnu home services shells)
             (gnu services)
             (gnu packages)
             (gnu packages base)
             (gnu packages gnome)
             (gnu packages emacs)
             (gnu packages shells)
             (guix packages)
             (guix utils)
             (guix build-system copy)
             (guix build-system go)
             (guix git-download)
             (packages)
             ((guix licenses) #:prefix license:)
             (guix gexp))

(home-environment
 (packages (append (map specification->package
                        (list
                         ;; Terminal
                         "zsh"
                         "zsh-syntax-highlighting"
                         "zsh-autopair"
                         "zsh-autosuggestions"
                         "tmux"
                         "direnv"

                         ;; Editors
                         "neovim"
                         "python-pynvim"

                         ;; CLI Apps
                         "mcron"                ; Cron Automation
                         "unzip"                ; Archive unzacker
                         "fzf"                  ; Fuzzy Finder
                         "ripgrep"              ; Better Grep
                         "the-silver-searcher"  ; Better Ripgrep
                         "bat"                  ; Better cat
                         "git"                  ; Version Control
                         "git-flow"             ; Code Workflow
                         "stow"                 ; Symbolic Links
                         "openssh"              ; SSH Connections
                         "nss-certs"            ; Internet Certificates
                         "glibc-locales"        ; important on foreign distro

                         ;; Spell Checking
                         "aspell"
                         "aspell-dict-en"
                         "aspell-dict-de"

                         ;; Fonts
                         "font-juliamono"
                         "font-jetbrains-mono"
                         "font-overpass"
                         "font-awesome"

                         ;; X-Tools
                         "picom"   ; Compositor
                         "fontmanager"
                         "xdg-utils"      ;; For xdg-open, etc
                         "xdg-dbus-proxy" ;; For Flatpak
                         "shared-mime-info"
                         "xset"
                         "xrandr"         ; Screen-Resolution
                         "redshift"       ; Day/Night - Red-Shift
                         "scrot"          ; CLI Screenshots
                         "xsel"           ; Manipulate Selections
                         "slock"          ; Screen Locker
                         "dmenu"          ; Menu Launcher
                         "rofi"           ; Application Launcher

                         ;; Applications
                         "alacritty"
                         "icecat"
                         "vlc"
                         "mpv"
                         "playerctl"
                         "gimp"

                         "flatpak"
                         "thunar"
                         ))
                   (list zsh-ohmyzsh
                         zsh-completions
                         awesome-copycats
                         tmux-tpm
                         emacs-next-pgtk-latest
                         neovim-config)
                   ))
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
                         `(".config/nvim/init.lua"
                           ,(local-file "config/nvim-init.lua" "nvim-init.lua"))
                         `(".config/nvim/lua/user"
                           ,(file-append neovim-config "/lua/user"))
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
                         `(".config/awesome/rc.lua"
                           ,(local-file "config/awesome.rc.lua"))
                         `(".config/awesome/themes"
                           ,(file-append awesome-copycats "/themes"))
                         `(".config/awesome/lain"
                           ,(file-append awesome-lain ""))
                         `(".config/awesome/freedesktop"
                           ,(file-append awesome-freedesktop ""))
                         `(".config/emacs"
                           ,(file-append emacs-doom ""))
                         `(".config/zsh/.p10k.zsh"
                           ,(local-file "config/.p10k.zsh" "p10k.zsh"))
                         `(".config/zsh/ohmyzsh"
                           ,(file-append zsh-ohmyzsh ""))
                         `(".cache/zsh/ohmyzsh/custom/themes"
                           ,(file-append zsh-powerlevel ""))
                         `(".cache/zsh/ohmyzsh/custom/plugins/zsh-completions"
                           ,(file-append zsh-completions ""))
                         `(".tmux/plugins/tpm"
                           ,(file-append tmux-tpm ""))
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
                     ("DOOMLOCALDIR" . "$XDG_CACHE_HOME/doom")
                     ("DOOMPROFILELOADPATH" . "$DOOMLOCALDIR/profiles")
                     ("DOOMPROFILELOADFILE" . "$DOOMLOCALDIR/profiles/load.el")
                     ("PATH" . ,(file-append emacs-doom (string-append "/bin" ":" (getenv "PATH"))))
                     ("ZSH" . ,(file-append zsh-ohmyzsh ""))
                     ("ZSH_CUSTOM" . "$HOME/.cache/zsh/ohmyzsh/custom")
                     ("SHELL" . ,(file-append zsh "/bin/zsh")))))))
