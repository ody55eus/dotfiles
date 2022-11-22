(define-module (admin)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system copy)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp))

(define-public nerdfetch
  (package
    (name "nerdfetch")
    (version "5.0.2")
    (home-page "https://codeberg.org/thatonecalculator/NerdFetch")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1jmy3f6lshl078svxdg0s5xrhwfdrz9lw0h5w1c34kmr45bh7mmy"))))
    (build-system copy-build-system)
    (arguments
      '(#:install-plan
            '(("nerdfetch" "bin/nerdfetch"))))
    (synopsis "System information script")
    (description "Nerdfetch is Neofetch with Nerd Fonts")
    (license license:expat)))
