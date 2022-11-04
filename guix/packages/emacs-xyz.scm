(define-module (emacs-xyz)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system go)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp))

(define-public emacs-doom
  (let ((commit "ceb985673ccd2be869bdde2ea4f41c84e9354f1e")
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
         "0axs8rqaaryc2xzhcvlg7x180jn19gj8h7s8srx67l1w6p20h08s"))))
     (build-system copy-build-system)
     (arguments
      '(#:install-plan
            '(("bin" "bin")
              ("." "share/doom/"))))
     (propagated-inputs
      (list binutils))
     (license license:expat)
     (synopsis "An Emacs framework for the stubborn martian hacker.")
     (description "Doom is a configuration framework for GNU Emacs tailored for
                   Emacs bankruptcy veterans who want less framework in their
                   frameworks, a modicum of stability (and reproducibility) from
                   their package manager, and the performance of a hand rolled config
                   (or better). It can be a foundation for your own config or a
                   resource for Emacs enthusiasts to learn more about our favorite
                   operating system.
                                Its design is guided by these mantras:
                                Gotta go fast. Startup and run-time performance are priorities. Doom goes beyond by modifying packages to be snappier and load lazier.
                                Close to metal. There's less between you and vanilla Emacs by design. That's less to grok and less to work around when you tinker. Internals ought to be written as if reading them were part of Doom's UX, and it is!
                                Opinionated, but not stubborn. Doom is about reasonable defaults and curated opinions, but use as little or as much of it as you like.
                                Your system, your rules. You know better. At least, Doom hopes so! It won't automatically install system dependencies (and will force plugins not to either). Rely on doom doctor to tell you what's missing.
                        Nix/Guix is a great idea! The Emacs ecosystem is temperamental. Things break and they break often. Disaster recovery should be a priority! Doom's package management should be declarative and your private config reproducible, and comes with a means to roll back releases and updates (still a WIP)."))))
