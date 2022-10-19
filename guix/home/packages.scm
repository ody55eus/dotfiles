(define-module (packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages shells)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system go)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp))

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

(define-public lazygit
  (let ((commit "36c6462a534ff328051bd81ed90eb626d1a59f09")
        (revision "1"))
    (package
      (name "lazygit")
      (version (git-version "git" revision commit))
      (home-page "https://github.com/jesseduffield/lazygit")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0ppl0pxfr7j7pmb9asipmkrknisf2v410a1ri0vxfrv0pa4qaayd"))))
      (build-system go-build-system)
      (arguments
       '(#:modules ((guix build go-build-system)
                    (guix build utils))
         #:phases
         (modify-phases %standard-phases
           (add-before 'build 'set-srcdir
             (lambda _
               (use-modules (guix build utils))
               (chdir "src")))
           (add-before 'build 'patch-go-files
             (lambda _
               (for-each (lambda (f)
                           (format #t "Patching ~a\n" f)
                           (substitute* f
                             (("github.com/jesseduffield/lazygit/")
                              "")))
                         (find-files "." "\\.go")
                         ))))))
      (license license:expat)
      (synopsis "")
      (description ""))))

(define-public neovim-config
  (let ((commit "fa51853c0890bf9992b6fc880025853772ecbdb0")
        (revision "1"))
    (package
     (name "neovim-config")
     (version (git-version "git" revision commit))
     (home-page "https://github.com/LunarVim/nvim-basic-ide")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0gz32birx2wv2cczsjqgr1lg78alws0qqf94xhqfhn59gnsv8sbj"))))
     (build-system copy-build-system)
     ;; (propagated-inputs
     ;;  (list lazygit))
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
