(define-module (nvim)
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
     (license license:gpl3)
     (synopsis "")
     (description ""))))
