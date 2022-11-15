(define-module (emacs)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages base)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix gexp))

(define-public emacs-next-latest
  (let ((commit "9ff2f0be32be621a0a1953cac2d552afebafe226")
        (revision "3"))
    (package
      (inherit emacs-next)
      (name "emacs-next-latest")
      (version (git-version "29.0.50" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.savannah.gnu.org/git/emacs.git/")
                      (commit commit)))
                (file-name (git-file-name name version))
                ;; (patches (search-patches "emacs-exec-path.patch"
                ;; "emacs-fix-scheme-indent-function.patch"
                ;; "emacs-native-comp-driver-options.patch"))
                (sha256
                 (base32
                  "0kj252ywhc0jw0j7mr3dwx4q5mi5rrlm4jlhc8mbx66ylfvxi9qg")))))))

(define-public emacs-next-pgtk-latest
  (package
    (inherit emacs-next-latest)
    (name "emacs-next-pgtk-latest")
    (arguments
     (substitute-keyword-arguments (package-arguments emacs-next)
       ((#:configure-flags flags
         ''())
        `(cons* "--with-pgtk"
                ,flags))))
    (propagated-inputs (package-propagated-inputs emacs-next-pgtk))))

(define-public emacs-lucid-latest
  (package
    (inherit emacs-next-latest)
    (name "emacs-lucid-latest")
    (arguments
     (substitute-keyword-arguments (package-arguments emacs-next)
       ((#:configure-flags flags
         ''())
        `(cons* "--without-gconf" "--without-gsettings"
                "--with-x-toolkit=lucid"
                ,flags))))
    (inputs (modify-inputs (package-inputs emacs-next)
              (delete "gtk+")
              (append libxaw))) ;for the Lucid toolkit
    (synopsis (string-append (package-synopsis emacs-next)
                             " (compiled --with-x-toolkit=lucid)"))))
