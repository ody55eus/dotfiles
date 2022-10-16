(use-modules (gnu home)
             (gnu home services)
             (gnu home services shells)
             (gnu services)
             (gnu packages)
             (gnu packages gnome)
             (gnu packages emacs)
             (gnu packages shells)
             (guix packages)
             (guix utils)
             (guix build-system copy)
             (guix git-download)
             ((guix licenses) #:prefix license:)
             (guix gexp))

(define-public emacs-doom
  (let ((commit "285b460c80e455fabaf036f0eb48575637586a46")
        (revision "1"))
    (package
     (name "emacs-doom")
     (version (git-version "3.0.0" revision commit))
     (home-page "https://github.com/doomemacs/doomemacs")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0xh5fzhk8qr23sgv931va0mjfgnw9qwxy5z4dp4fdr4dvnavd6vz"))))
     (build-system copy-build-system)
     (inputs
      (package-inputs zsh))
     (license license:expat)
     (synopsis "An Emacs framework for the stubborn martian hacker.")
     (description "Doom is a configuration framework for GNU Emacs tailored for Emacs bankruptcy veterans who want less framework in their frameworks, a modicum of stability (and reproducibility) from their package manager, and the performance of a hand rolled config (or better). It can be a foundation for your own config or a resource for Emacs enthusiasts to learn more about our favorite operating system.

                                Its design is guided by these mantras:

                                Gotta go fast. Startup and run-time performance are priorities. Doom goes beyond by modifying packages to be snappier and load lazier.

                                Close to metal. There's less between you and vanilla Emacs by design. That's less to grok and less to work around when you tinker. Internals ought to be written as if reading them were part of Doom's UX, and it is!

                                Opinionated, but not stubborn. Doom is about reasonable defaults and curated opinions, but use as little or as much of it as you like.

                                Your system, your rules. You know better. At least, Doom hopes so! It won't automatically install system dependencies (and will force plugins not to either). Rely on doom doctor to tell you what's missing.

                                Nix/Guix is a great idea! The Emacs ecosystem is temperamental. Things break and they break often. Disaster recovery should be a priority! Doom's package management should be declarative and your private config reproducible, and comes with a means to roll back releases and updates (still a WIP).
                                "))))

(define-public zsh-powerlevel
  (let ((commit "be3724bc806a2dd7fbcb281a153b11ab19d8923d")
        (revision "1"))
    (package
     (name "zsh-powerlevel10k")
     (version (git-version "1.16.1" revision commit))
     (home-page "https://github.com/romkatv/powerlevel10k")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "095lk51yf3bmh11hv4lb019h77zv9m48cfkh8qd0w2rqrcl6p2sr"))))
     (build-system copy-build-system)
     (license license:expat)
     (synopsis "A Zsh theme")
     (description "Powerlevel10k is a theme for Zsh. It emphasizes speed, flexibility and out-of-the-box experience."))))

(define-public zsh-ohmyzsh
  (let ((commit "4c82a2eedf0c43d47601ffa8b0303ed1326fab8f")
        (revision "1"))
    (package
     (name "zsh-ohmyzsh")
     (version (git-version "1.0" revision commit))
     (home-page "https://github.com/ohmyzsh/ohmyzsh")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "01gxpyinagaq7dqwjrdcbvdw5fb7ghfglb3spw85z90mjlwlrwbs"))))
     (build-system copy-build-system)
     ;; (arguments
     ;;  `(#:phases
     ;;    (modify-phases %standard-phases
     ;;      (add-before 'install 'remove-custom-theme
     ;;          (lambda _
     ;;            (delete-file-recursively "/custom/themes")))
     ;;    )))
     (propagated-inputs
      (list zsh zsh-powerlevel))
     (license license:expat)
     (synopsis "Oh My Zsh is an open source, community-driven framework for managing your zsh configuration.")
     (description "A delightful community-driven (with 2,000+ contributors) framework for managing your zsh configuration. Includes 300+ optional plugins (rails, git, macOS, hub, docker, homebrew, node, php, python, etc), 140+ themes to spice up your morning, and an auto-update tool so that makes it easy to keep up with the latest updates from the community."))))

(define-public zsh-completions
  (let ((commit "10b46f923a81146d4ab45764ac1cba7d5fd958b2")
        (revision "1"))
    (package
     (name "zsh-completions")
     (version (git-version "1.0" revision commit))
     (home-page "https://github.com/zsh-users/zsh-completions")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "18djyiq2i3qcamdk9v3dn7cczxwx7hdmfk64vbb9gar0kx9vg66n"))))
     (build-system copy-build-system)
     ;; (arguments
     ;;  `(#:phases
     ;;    (modify-phases %standard-phases
     ;;      (add-before 'install 'remove-custom-theme
     ;;          (lambda _
     ;;            (delete-file-recursively "/custom/themes")))
     ;;    )))
     (propagated-inputs
      (list zsh zsh-powerlevel))
     (license license:expat)
     (synopsis "Oh My Zsh is an open source, community-driven framework for managing your zsh configuration.")
     (description "A delightful community-driven (with 2,000+ contributors) framework for managing your zsh configuration. Includes 300+ optional plugins (rails, git, macOS, hub, docker, homebrew, node, php, python, etc), 140+ themes to spice up your morning, and an auto-update tool so that makes it easy to keep up with the latest updates from the community."))))

(define-public awesome-freedesktop
  (let ((commit "c82ad2960c5f0c84e765df68554c266ea7e9464d")
        (revision "1"))
    (package
     (name "awesome-freedesktop")
     (version (git-version "git" revision commit))
     (home-page "https://github.com/lcpz/awesome-freedesktop")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0jw0p8hl4f5jdcjpafhyg0z2p9ypj2629h0wwqnqalfgqh4js2wm"))))
     (build-system copy-build-system)
     (license license:gpl2)
     (synopsis "")
     (description ""))))

(define-public awesome-lain
  (let ((commit "c489aa63acc1364851e0e51152be3db5c75e145d")
        (revision "1"))
    (package
     (name "awesome-lain")
     (version (git-version "git" revision commit))
     (home-page "https://github.com/lcpz/lain")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1mw2b7jlvz04xsl9a835rgndpj3z2vy931fzjiwq8bpbw2p27kdk"))))
     (build-system copy-build-system)
     (license license:gpl2)
     (synopsis "")
     (description ""))))

(define-public awesome-copycats
  (let ((commit "16d85b1123af20162801e5ab04786f9a7c760273")
        (revision "1"))
    (package
     (name "awesome-copycats")
     (version (git-version "git" revision commit))
     (home-page "https://github.com/lcpz/awesome-copycats")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1xihddnk5kv184rx26qachckljk4q2sg7xnf1kvz2s96z5bycm0j"))))
     (build-system copy-build-system)
     (propagated-inputs
      (list awesome-lain
            awesome-freedesktop))
     (license license:cc-by-sa4.0)
     (synopsis "")
     (description ""))))

(define-public tmux-tpm
  (let ((commit "b699a7e01c253ffb7818b02d62bce24190ec1019")
        (revision "1"))
    (package
     (name "tmux-tpm")
     (version (git-version "git" revision commit))
     (home-page "https://github.com/tmux-plugins/tpm")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1395fv70gxkpqswnraw50fcaawnjn91j4a44yzz1c3vmm3jp4r38"))))
     (build-system copy-build-system)
     (license license:gpl3)
     (synopsis "")
     (description ""))))

(define-public neovim-config
  (let ((commit "6c76b99921d6b43d31c5e331e0886d6118f6df0a")
        (revision "1"))
    (package
     (name "neovim-config")
     (version (git-version "git" revision commit))
     (home-page "https://github.com/LunarVim/Neovim-from-scratch")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "159jnyrjklc40phpw35m59lqhw90amzqxy9585snx8hb42qxds0f"))))
     (build-system copy-build-system)
     (license license:gpl3)
     (synopsis "")
     (description ""))))

(define-public emacs-next-pgtk-latest
  (let ((commit "9ff2f0be32be621a0a1953cac2d552afebafe226")
        (revision "3"))
    (package
     (inherit emacs-next-pgtk)
     (name "emacs-next-pgtk-latest")
     (version (git-version "29.0.50" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.savannah.gnu.org/git/emacs.git/")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0kj252ywhc0jw0j7mr3dwx4q5mi5rrlm4jlhc8mbx66ylfvxi9qg"))))
     (arguments
      (substitute-keyword-arguments (package-arguments emacs-next)
                                    ((#:configure-flags flags ''())
                                     `(cons* "--with-pgtk" ,flags))))
     ;; (propagated-inputs
     ;;  (list gsettings-desktop-schemas glib-networking))
     (inputs
      (package-inputs emacs-next)))))

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
                         `(".config/doom/packages.el"
                           ,(local-file "../../doom/.doom.d/packages.el" "packages.el"))
                         `(".config/doom/org-workflow.el"
                           ,(local-file "../../doom/.doom.d/org-workflow.el" "org-workflow.el"))
                         `(".config/doom/config.el"
                           ,(local-file "../../doom/.doom.d/config.el" "config.el"))
                         `(".config/doom/init.el"
                           ,(local-file "../../doom/.doom.d/init.el" "init.el"))
                         ;; `(".config/doom/profiles.el"
                         ;;   ,(local-file "../../doom/.doom.d/profiles.el" "profiles.el"))
                         `(".vim/.vimrc"
                           ,(local-file "config/.vim/vimrc" "vimrc"))
                         `(".config/bat/config"
                           ,(local-file "config/bat.config"))
                         `(".config/git/config"
                           ,(local-file "config/git.config"))
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
                         `(".tmux.conf"
                           ,(local-file "config/.tmux.conf" "tmux.conf"))))

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
