(define-module (fonts)
  #:use-module (ice-9 regex)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system font)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages c)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages xorg))

(define-public font-nerd-ibm-plex-mono
  (package
    (name "font-nerd-ibm-plex-mono")
    (version "2.2.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/ryanoasis/nerd-fonts/releases/"
                       "download/v" version "/IBMPlexMono.zip"))
       (sha256
        (base32 "0fadrcjiribzk3vcc3k80bddffr63b28ikrv9qrppbkwc846xiq5"))))
    (build-system font-build-system)
    ;; (arguments
    ;;  `(#:phases
    ;;    (modify-phases %standard-phases
    ;;      (add-before 'install-license-files 'change-directory-to-archive-root
    ;;        ;; Find the license file outside of the default subdirectory.
    ;;        (lambda _
    ;;          (chdir "..")
    ;;          #t))
    ;;      (replace 'install-license-files
    ;;        (lambda* (#:key outputs #:allow-other-keys)
    ;;          (let* ((out (assoc-ref outputs "out"))
    ;;                 (doc (string-append out "/share/doc/" ,name "-" ,version)))
    ;;            (install-file "OFL.txt" doc)
    ;;            #t))))))
    (home-page "https://github.com/ryanoasis/nerd-fonts")
    (synopsis "Mono typeface for developers")
    (description "")
    (license license:asl2.0)))

(define-public font-nerd-overpass
  (package
    (name "font-nerd-overpass")
    (version "2.2.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/ryanoasis/nerd-fonts/releases/"
                       "download/v" version "/Overpass.zip"))
       (sha256
        (base32 "0kxw9qqp9a9nh5ir0ll34965k15nyrs708ffry6bvccy6sl723n4"))))
    (build-system font-build-system)
    ;; (arguments
    ;;  `(#:phases
    ;;    (modify-phases %standard-phases
    ;;      (add-before 'install-license-files 'change-directory-to-archive-root
    ;;        ;; Find the license file outside of the default subdirectory.
    ;;        (lambda _
    ;;          (chdir "..")
    ;;          #t))
    ;;      (replace 'install-license-files
    ;;        (lambda* (#:key outputs #:allow-other-keys)
    ;;          (let* ((out (assoc-ref outputs "out"))
    ;;                 (doc (string-append out "/share/doc/" ,name "-" ,version)))
    ;;            (install-file "OFL.txt" doc)
    ;;            #t))))))
    (home-page "https://github.com/ryanoasis/nerd-fonts")
    (synopsis "Mono typeface for developers")
    (description "")
    (license license:asl2.0)))

(define-public font-nerd-terminus
  (package
    (name "font-nerd-terminus")
    (version "2.2.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/ryanoasis/nerd-fonts/releases/"
                       "download/v" version "/Terminus.zip"))
       (sha256
        (base32 "1qa9hjjjw3xn4qk5sqifxd87q5xgawwd9d2yh62b9n1rpgi37cip"))))
    (build-system font-build-system)
    ;; (arguments
    ;;  `(#:phases
    ;;    (modify-phases %standard-phases
    ;;      (add-before 'install-license-files 'change-directory-to-archive-root
    ;;        ;; Find the license file outside of the default subdirectory.
    ;;        (lambda _
    ;;          (chdir "..")
    ;;          #t))
    ;;      (replace 'install-license-files
    ;;        (lambda* (#:key outputs #:allow-other-keys)
    ;;          (let* ((out (assoc-ref outputs "out"))
    ;;                 (doc (string-append out "/share/doc/" ,name "-" ,version)))
    ;;            (install-file "OFL.txt" doc)
    ;;            #t))))))
    (home-page "https://github.com/ryanoasis/nerd-fonts")
    (synopsis "Mono typeface for developers")
    (description "")
    (license license:asl2.0)))
