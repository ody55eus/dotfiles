(define-module (zsh-xyz)
  #:use-module (gnu packages base)
  #:use-module (gnu packages shells)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system go)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp))


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
     (arguments
      '(#:install-plan
        '(("." "share/ohmyzsh"))))
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
     (arguments
      '(#:install-plan
        '(("." "share/zsh/plugins/zsh-completion"))))
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
     (arguments
      '(#:install-plan
        '(("." "share/zsh/plugins/p10k"))))
     (license license:expat)
     (synopsis "A Zsh theme")
     (description "Powerlevel10k is a theme for Zsh. It emphasizes speed, flexibility and out-of-the-box experience."))))
