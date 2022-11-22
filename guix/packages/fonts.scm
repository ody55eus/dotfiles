(define-module (fonts)
  #:use-module (ice-9 regex)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system font)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system trivial)
  #:use-module ((gnu packages autotools)
                #:select (autoconf automake))
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
  #:use-module ((gnu packages tex)
                #:select (texlive-bin))
  #:use-module (gnu packages xorg))

(define-public font-nerd-fonts
  (package
    (name "font-nerd-fonts")
    (version "2.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ryanoasis/nerd-fonts")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "09i467hyskvzj2wn5sj6shvc9pb0a0rx5iknjkkkbg1ng3bla7nm"))))
    (build-system font-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before 'install 'make-files-writable
                    (lambda _
                      (for-each make-file-writable
                                (find-files "." ".*\\.(otf|otc|ttf|ttc)$")) #t)))))
    (home-page "https://www.nerdfonts.com/")
    (synopsis "Iconic font aggregator, collection, and patcher")
    (description
     "Nerd Fonts patches developer targeted fonts with a high number
of glyphs (icons). Specifically to add a high number of extra glyphs
from popular ‘iconic fonts’ such as Font Awesome, Devicons, Octicons,
and others.")
    (license license:expat)))

(define-public font-font-awesome
  (package
    (name "font-font-awesome")
    (version "5.11.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/FortAwesome/Font-Awesome")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ya14lgx5mgpjbnw6sss3a2c99n6cw6xryd0xj8rbjwbr2gmrf1q"))))
    (build-system font-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before 'install 'make-files-writable
                    (lambda _
                      (for-each make-file-writable
                                (find-files "." ".*\\.(otf|otc|ttf|ttc)$")) #t)))))
    (home-page "https://fontawesome.com")
    (synopsis "The iconic SVG, font, and CSS toolkit")
    (description
     "The internet's most popular icon toolkit has been redesigned and
built from scratch. On top of this, features like icon font ligatures,
an SVG framework, official NPM packages for popular frontend libraries
like React, and access to a new CDN.")
    (license #f)))

(define-public font-nerd-ibm-plex-mono
  (package
    (name "font-nerd-ibm-plex-mono")
    (version "2.2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/ryanoasis/nerd-fonts/releases/"
                    "download/v" version "/IBMPlexMono.zip"))
              (sha256
               (base32
                "0fadrcjiribzk3vcc3k80bddffr63b28ikrv9qrppbkwc846xiq5"))))
    (build-system font-build-system)
    ;; (arguments
    ;; `(#:phases
    ;; (modify-phases %standard-phases
    ;; (add-before 'install-license-files 'change-directory-to-archive-root
    ;; ;; Find the license file outside of the default subdirectory.
    ;; (lambda _
    ;; (chdir "..")
    ;; #t))
    ;; (replace 'install-license-files
    ;; (lambda* (#:key outputs #:allow-other-keys)
    ;; (let* ((out (assoc-ref outputs "out"))
    ;; (doc (string-append out "/share/doc/" ,name "-" ,version)))
    ;; (install-file "OFL.txt" doc)
    ;; #t))))))
    (home-page "https://github.com/ryanoasis/nerd-fonts")
    (synopsis "Mono typeface for developers")
    (description "")
    (license license:asl2.0)))

(define-public font-nerd-overpass
  (package
    (name "font-nerd-overpass")
    (version "2.2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/ryanoasis/nerd-fonts/releases/"
                    "download/v" version "/Overpass.zip"))
              (sha256
               (base32
                "0kxw9qqp9a9nh5ir0ll34965k15nyrs708ffry6bvccy6sl723n4"))))
    (build-system font-build-system)
    ;; (arguments
    ;; `(#:phases
    ;; (modify-phases %standard-phases
    ;; (add-before 'install-license-files 'change-directory-to-archive-root
    ;; ;; Find the license file outside of the default subdirectory.
    ;; (lambda _
    ;; (chdir "..")
    ;; #t))
    ;; (replace 'install-license-files
    ;; (lambda* (#:key outputs #:allow-other-keys)
    ;; (let* ((out (assoc-ref outputs "out"))
    ;; (doc (string-append out "/share/doc/" ,name "-" ,version)))
    ;; (install-file "OFL.txt" doc)
    ;; #t))))))
    (home-page "https://github.com/ryanoasis/nerd-fonts")
    (synopsis "Mono typeface for developers")
    (description "")
    (license license:asl2.0)))

(define-public font-nerd-terminus
  (package
    (name "font-nerd-terminus")
    (version "2.2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/ryanoasis/nerd-fonts/releases/"
                    "download/v" version "/Terminus.zip"))
              (sha256
               (base32
                "1qa9hjjjw3xn4qk5sqifxd87q5xgawwd9d2yh62b9n1rpgi37cip"))))
    (build-system font-build-system)
    ;; (arguments
    ;; `(#:phases
    ;; (modify-phases %standard-phases
    ;; (add-before 'install-license-files 'change-directory-to-archive-root
    ;; ;; Find the license file outside of the default subdirectory.
    ;; (lambda _
    ;; (chdir "..")
    ;; #t))
    ;; (replace 'install-license-files
    ;; (lambda* (#:key outputs #:allow-other-keys)
    ;; (let* ((out (assoc-ref outputs "out"))
    ;; (doc (string-append out "/share/doc/" ,name "-" ,version)))
    ;; (install-file "OFL.txt" doc)
    ;; #t))))))
    (home-page "https://github.com/ryanoasis/nerd-fonts")
    (synopsis "Mono typeface for developers")
    (description "")
    (license license:asl2.0)))

(define-public lcdf-typetools
  (package
    (name "lcdf-typetools")
    (version "2.108")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kohler/lcdf-typetools")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0a6jqaqwq43ldjjjlnsh6mczs2la9363qav7v9fyrfzkfj8kw9ad"))))
    (inputs
     `(("texlive-bin" ,texlive-bin)
       ("autoconf" ,autoconf)
       ("automake" ,automake)))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list
        (string-append
         "--with-kpathsea="
         (assoc-ref %build-inputs "texlive-bin")))))
    (home-page "http://www.lcdf.org/type/")
    (synopsis
     "Utilities for manipulating various fonts")
    (description
     "LCDF Typetools comprises several programs for manipulating
PostScript Type 1, Type 1 Multiple Master, OpenType, and TrueType
fonts.")
    (license license:gpl2+)))
