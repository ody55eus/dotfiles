(define-module (emacs)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp))


(define-public emacs-next-pgtk-latest
  (let ((commit "9ff2f0be32be621a0a1953cac2d552afebafe226")
        (revision "3"))
    (package
     (inherit emacs-next-pgtk)
     (name "emacs-next-pgtk-latest")
     (version (git-version "29.0.50" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.savannah.gnu.org/git/emacs.git/")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0kj252ywhc0jw0j7mr3dwx4q5mi5rrlm4jlhc8mbx66ylfvxi9qg"))))
     (arguments
      (substitute-keyword-arguments (package-arguments emacs-next)
                                    ((#:configure-flags flags ''())
                                     `(cons* "--with-pgtk" ,flags))))
     ;; (propagated-inputs
     ;;  (list gsettings-desktop-schemas glib-networking))
     (inputs
      (package-inputs emacs-next)))))
