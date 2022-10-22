(define-module (nvim)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages python)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages vim)
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
     (arguments
      '(#:install-plan
            '(("lua" "share/"))))
     (license license:gpl3)
     (synopsis "")
     (description ""))))

(define-public neovim-lunarvim
  (let ((commit "36c8bdee9ff59a0a63c1edfc445b5eb2886cf246")
        (branch "master")
        (revision "1"))
    (package
     (name "neovim-lunarvim")
     (version (git-version "git" revision commit))
     (home-page "https://github.com/LunarVim/LunarVim")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "184idilx702chshcp0y7a6r7y5gzn0n85ai33ayb71v64fg6vrkl"))))
     (build-system copy-build-system)
     (arguments
      (list #:install-plan
            #~'(("lua" "share/lua")
                ("utils/bin/lvim.template" "bin/lvim.template")
                ("utils/installer/config.example.lua" "config/lvim/config.lua"))))
    ;; (inputs (list
    ;;          neovim
    ;;          python-pynvim
    ;;          git))
     (license license:gpl3)
     (synopsis "")
     (description ""))))
