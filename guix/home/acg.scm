(use-modules (gnu home)
             (gnu home services)
             (gnu home services shells)
             (gnu services)
             (gnu packages)
             (gnu packages base)
             (gnu packages shells)
             (jpg packages emacs)
             (guix packages)
             (guix build-system copy)
             (guix git-download)
             ((guix licenses) #:prefix license:)
             (guix gexp))

(define-public emacs-doom
  (let ((commit "7e50f239c46ea17429f159fb543c0d793543c06e")
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
         "1zl2zqs0921f15k2a9f647qxnb6vvqyw3f81wiwhg3njc4lzc0yx"))))
     (build-system copy-build-system)
	 (propagated-inputs
	   (list binutils))
     (license license:expat)
     (synopsis "An Emacs framework for the stubborn martian hacker.")
     (description "Doom is a configuration framework for GNU Emacs tailored for Emacs bankruptcy veterans who want less framework in their frameworks, a modicum of stability (and reproducibility) from their package manager, and the performance of a hand rolled config (or better). It can be a foundation for your own config or a resource for Emacs enthusiasts to learn more about our favorite operating system.

Its design is guided by these mantras:

Gotta go fast. Startup and run-time performance are priorities. Doom goes beyond by modifying packages to be snappier and load lazier.

Close to metal. There's less between you and vanilla Emacs by design. That's less to grok and less to work around when you tinker. Internals ought to be written as if reading them were part of Doom's UX, and it is!

Opinionated, but not stubborn. Doom is about reasonable defaults and curated opinions, but use as little or as much of it as you like.

Your system, your rules. You know better. At least, Doom hopes so! It won't automatically install system dependencies (and will force plugins not to either). Rely on doom doctor to tell you what's missing.

Nix/Guix is a great idea! The Emacs ecosystem is temperamental. Things break and they break often. Disaster recovery should be a priority! Doom's package management should be declarative and your private config reproducible, and comes with a means to roll back releases and updates (still a WIP)."))))

(define-public zsh-powerlevel
  (let ((commit "5ee784787fe3c1855ee6f365cbf045712843989e")
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
         "1pm7471gxczclrdkf3269qj3bkq2a7kfcig4apw21sd41x7jyiy6"))))
     (build-system copy-build-system)
     (inputs
      (package-inputs zsh))
     (license license:expat)
     (synopsis "A Zsh theme")
     (description "Powerlevel10k is a theme for Zsh. It emphasizes speed, flexibility and out-of-the-box experience."))))

(define-public zsh-ohmyzsh
  (let ((commit "570158e464c9f57ab03c4162b4e6853b2c7c650d")
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
         "14waa2zc66ywcdz6dkcgisg1axjp13cqh0piwixhla2iwy5aq4zy"))))
     (build-system copy-build-system)
    ;; (arguments
    ;;  `(#:phases
    ;;    (modify-phases %standard-phases
    ;;      (add-before 'install 'remove-custom-theme
    ;;          (lambda _
    ;;            (delete-file-recursively "/custom/themes")))
    ;;    )))
     (propagated-inputs
      (list zsh-powerlevel))
     (inputs
      (package-inputs zsh))
     (license license:expat)
     (synopsis "Oh My Zsh is an open source, community-driven framework for managing your zsh configuration.")
     (description "A delightful community-driven (with 2,000+ contributors) framework for managing your zsh configuration. Includes 300+ optional plugins (rails, git, macOS, hub, docker, homebrew, node, php, python, etc), 140+ themes to spice up your morning, and an auto-update tool so that makes it easy to keep up with the latest updates from the community."))))

(home-environment
 (packages (append (map specification->package
                        (list
                                      "zsh"
                                      "zsh-syntax-highlighting"
                                      "zsh-autopair"
                                      "zsh-autosuggestions"
                                      "direnv"

                                      "emacs-next-pgtk-latest"
                                      "neovim"
                                      "tmux"

                                      "mcron"
                                      "unzip"
                                      "fzf"
                                      "ripgrep"
                                      "the-silver-searcher"
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
                                      ))
                   (list zsh-ohmyzsh
						 emacs-doom)
						 ; emacs-next-lucid-latest)

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
                              `(".config/alacritty/alacritty.yml"
                                ,(local-file "config/alacritty.yml"))
                              `(".config/awesome/rc.lua"
                                ,(local-file "config/awesome.rc.lua"))
                              `(".config/emacs"
                                ,(file-append emacs-doom ""))
                              `(".config/zsh/.p10k.zsh"
                                ,(local-file "config/.p10k.zsh" "p10k.zsh"))
                              `(".config/zsh/ohmyzsh"
                                ,(file-append zsh-ohmyzsh ""))
                              `(".cache/zsh/ohmyzsh/custom/themes"
                                ,(file-append zsh-powerlevel ""))
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
                          ("DOOMLOCALDIR" . "$XDG_CACHE_HOME/doom")
                          ("DOOMPROFILELOADPATH" . "$DOOMLOCALDIR/profiles")
                          ("DOOMPROFILELOADFILE" . "$DOOMLOCALDIR/profiles/load.el")
                          ("PATH" . ,(file-append emacs-doom "/bin:$PATH"))
                          ("ZSH" . ,(file-append zsh-ohmyzsh ""))
                          ("ZSH_CUSTOM" . "$HOME/.cache/zsh/ohmyzsh/custom")
                          ("SHELL" . ,(file-append zsh "/bin/zsh")))))))
