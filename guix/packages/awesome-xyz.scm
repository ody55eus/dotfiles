(define-module (awesome-xyz)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gnome)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system go)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp))

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
