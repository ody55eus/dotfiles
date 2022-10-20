(define-module (tmux)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system copy)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp))

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
