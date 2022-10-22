(define-module (emacs)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages xorg)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp))

(define-public emacs-lucid-latest
  (let ((commit "f61db42fc580fb671016c77be942506d9081ac2c")
        (revision "3"))
    (package 
      (inherit emacs-next-pgtk)
      (name "emacs-lucid-latest")
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
          "0n25hzxv72kfavqpifhdpqchx398mqzvpiypdrbfyc8k66jxl2f9"))))
      (inputs (append `(("libxaw" ,libxaw)) ; for the Lucid toolkit
                      (package-inputs emacs-next)))
      (arguments
       (substitute-keyword-arguments (package-arguments emacs-next)
                                     ((#:configure-flags flags ''())
                                      `(cons*
                                        "--with-x-toolkit=lucid"
                                        ,flags))))
      (synopsis (string-append (package-synopsis emacs-next)
                               " (compiled --with-x-toolkit=lucid and --without-gconf/xsettings")))))

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
