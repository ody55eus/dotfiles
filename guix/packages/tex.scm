(define-module (tex)
  #:use-module ((fonts) #:select (lcdf-typetools))
  #:use-module (gnu packages tex)
  #:use-module ((guix build-system python) #:select (python-build-system))
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system texlive)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module ((gnu packages python) #:select (python-wrapper))
  #:use-module ((gnu packages fontutils) #:select (fontforge))
  #:use-module ((guix licenses) #:prefix license:))

(define-public texlive-latex-moderncv
  (package
    (name "texlive-latex-moderncv")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xdanaux/moderncv")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1slgjsyiagglpzx4fqwmhbq6bnz40ii5kl8g2vbh144nnlql0smj"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/latex/moderncv")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "https://ctan.org/pkg/moderncv")
    (synopsis "A modern curriculum vitae class")
    (description
     "The class provides facilities for typesetting modern curriculums
vitae, both in a classic and in a casual style.  It is fairly
customizable, allowing you to define your own style by changing the
colours, the fonts, etc.")
    (license license:lppl1.3+)))

(define-public texlive-latex-fontawesome
  (package
    (name "texlive-latex-fontawesome")
    (version "4.6.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xdanaux/fontawesome-latex")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0m5fm1hj747braq7g7j8zbld7fq71ikdw5glva3w54yzqkgwcx6k"))))
    (inputs
     `(("fontforge" ,fontforge)
       ("lcdf-typetools" ,lcdf-typetools)
       ("texlive-bin" ,texlive-bin)
       ("texlive-latex-type1cm" ,texlive-latex-type1cm)))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (invoke "python" "generate_tex_bindings.py" ,version)
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             ;; install fonts
             (let* ((font-dir
                     (lambda (dir)
                       (string-append
                        (assoc-ref %outputs "out")
                        "/share/texmf-dist/fonts/" dir "/fontawesome")))
                    (tfm  `("tfm" ,(font-dir "tfm/public")))
                    (enc  `("enc" ,(font-dir "enc/pdftex/public")))
                    (otf  `("otf" ,(font-dir "opentype/public")))
                    (mmap `("map" ,(font-dir "map/dvips")))
                    (t1   `("pfb" ,(font-dir "type1/public"))))
               (for-each
                (lambda (p)
                  (let ((re (car p))
                        (dir (cadr p)))
                    (for-each (lambda (f) (install-file f dir))
                              (find-files "." (string-append ".*\\." re "$")))))
                (list tfm enc otf mmap t1)))

             ;; install tex and fd
             (let ((dest
                    (string-append
                     (assoc-ref outputs "out")
                     "/share/texmf-dist/tex/latex/fontawesome")))
               (for-each (lambda (f) (install-file f dest))
                         (find-files "." ".*\\.(tex|sty|fd)$")))
             #t))
         (delete 'check)
         (delete 'wrap))))
    (home-page "http://www.ctan.org/pkg/fontawesome")
    (synopsis
     "Font containing web-related icons")
    (description
     "The package offers access to the large number of web-related
icons provided by the included
http://fortawesome.github.io/Font-Awesome/font.  The package requires
the package, fontspec, if run with XeTeX or LuaTeX.")
    (license license:lppl1.3+)))
