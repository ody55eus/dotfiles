(define-module (guix packages python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix build-system trivial)
  #:use-module (guix packages python36-build-system)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages time))


(define (python-version python)
  (let* ((version     (last (string-split python #\-)))
         (components  (string-split version #\.))
         (major+minor (take components 2)))
    (string-join major+minor ".")))

(define (python-output outputs)
  "Return the path of the python output, if there is one, or fall-back to out."
  (or (assoc-ref outputs "python")
      (assoc-ref outputs "out")))

(define (site-packages inputs outputs)
  "Return the path of the current output's Python site-package."
  (let* ((out (python-output outputs))
         (python (assoc-ref inputs "python")))
    (string-append out "/lib/python" (python-version python) "/site-packages")))

(define (add-installed-pythonpath inputs outputs)
  "Prepend the site-package of OUTPUT to GUIX_PYTHONPATH.  This is useful when
running checks after installing the package."
  (setenv "GUIX_PYTHONPATH" (string-append (site-packages inputs outputs) ":"
                                           (getenv "GUIX_PYTHONPATH"))))

(define* (add-install-to-pythonpath #:key inputs outputs #:allow-other-keys)
  "A phase that just wraps the 'add-installed-pythonpath' procedure."
  (add-installed-pythonpath inputs outputs))

(define (search-patch file-name)
  "Search the patch FILE-NAME.  Raise an error if not found."
  (or (search-path (%patch-path) file-name)
      (raise (formatted-message (G_ "~a: patch not found")
                                file-name))))

(define-syntax-rule (search-patches file-name ...)
  "Return the list of absolute file names corresponding to each
FILE-NAME found in %PATCH-PATH."
  (list (search-patch file-name) ...))

(define %patch-path
  (make-parameter
   (append
    (list (string-append (getenv "HOME") "/.dotfiles/guix/packages/patches"))
    (%patch-path))))

;; From guix repository commit:
;; d66146073def03d1a3d61607bc6b77997284904b
(define-public python-3.6
  (package (inherit python-2)
    (name "python")
    (version "3.6.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.python.org/ftp/python/"
                                  version "/Python-" version ".tar.xz"))
              (patches (search-patches
                        "python-fix-tests.patch"
                        "python-3-fix-tests.patch"
                        "python-3-deterministic-build-info.patch"
                        "python-3-search-paths.patch"))
              (patch-flags '("-p0"))
              (sha256
               (base32
                "19l7inxm056jjw33zz97z0m02hsi7jnnx5kyb76abj5ml4xhad7l"))
              (snippet
               '(begin
                  (for-each delete-file
                            '("Lib/ctypes/test/test_structures.py" ; fails on aarch64
                              "Lib/ctypes/test/test_win32.py" ; fails on aarch64
                              "Lib/test/test_fcntl.py")) ; fails on aarch64
                  #t))))
    (arguments
     (substitute-keyword-arguments (package-arguments python-2)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'patch-timestamp-for-pyc-files
             (lambda _
               ;; We set DETERMINISTIC_BUILD to only override the mtime when
               ;; building with Guix, lest we break auto-compilation in
               ;; environments.
               (setenv "DETERMINISTIC_BUILD" "1")
               (substitute* "Lib/py_compile.py"
                 (("source_stats\\['mtime'\\]")
                  "(1 if 'DETERMINISTIC_BUILD' in os.environ else source_stats['mtime'])"))

               ;; Use deterministic hashes for strings, bytes, and datetime
               ;; objects.
               (setenv "PYTHONHASHSEED" "0")

               ;; Reset mtime when validating bytecode header.
               (substitute* "Lib/importlib/_bootstrap_external.py"
                 (("source_mtime = int\\(source_stats\\['mtime'\\]\\)")
                  "source_mtime = 1"))
               #t))
           ;; These tests fail because of our change to the bytecode
           ;; validation.  They fail because expected exceptions do not get
           ;; thrown.  This seems to be no problem.
           (add-after 'unpack 'disable-broken-bytecode-tests
             (lambda _
               (substitute* "Lib/test/test_importlib/source/test_file_loader.py"
                 (("test_bad_marshal")
                  "disable_test_bad_marshal")
                 (("test_no_marshal")
                  "disable_test_no_marshal")
                 (("test_non_code_marshal")
                  "disable_test_non_code_marshal"))
               #t))
           ;; Unset DETERMINISTIC_BUILD to allow for tests that check that
           ;; stale pyc files are rebuilt.
           (add-before 'check 'allow-non-deterministic-compilation
             (lambda _ (unsetenv "DETERMINISTIC_BUILD") #t))
           ;; We need to rebuild all pyc files for three different
           ;; optimization levels to replace all files that were not built
           ;; deterministically.

           ;; FIXME: Without this phase we have close to 2000 files that
           ;; differ across different builds of this package.  With this phase
           ;; there are about 500 files left that differ.
           (delete 'rebuild-bytecode)
           (add-after 'install 'rebuild-bytecode
             (lambda* (#:key outputs #:allow-other-keys)
               (setenv "DETERMINISTIC_BUILD" "1")
               (let ((out (assoc-ref outputs "out")))
                 (for-each
                  (lambda (opt)
                    (format #t "Compiling with optimization level: ~a\n"
                            (if (null? opt) "none" (car opt)))
                    (for-each (lambda (file)
                                (apply invoke
                                       `(,(string-append out "/bin/python3")
                                         ,@opt
                                         "-m" "compileall"
                                         "-f" ; force rebuild
                                         ;; Don't build lib2to3, because it's Python 2 code.
                                         ;; Also don't build obviously broken test code.
                                         "-x" "(lib2to3|test/bad.*)"
                                         ,file)))
                              (find-files out "\\.py$")))
                  (list '() '("-O") '("-OO")))
                 #t)))
           (delete 'check)
           (delete 'remove-tests)
           ))))
    (native-search-paths
     (list (search-path-specification
            (variable "PYTHONPATH")
            (files (list (string-append "lib/python"
                                        (version-major+minor version)
                                        "/site-packages"))))))))

(define-public python-3 python-3.6)
(define-public python python-3)

(define* (wrap-python3 python
                       #:optional
                       (name (string-append (package-name python) "-wrapper")))
  (package/inherit python
    (name name)
    (source #f)
    (build-system trivial-build-system)
    (outputs '("out"))
    (inputs `(("bash" ,bash)))
    (propagated-inputs `(("python" ,python)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
         (begin
           (use-modules (guix build utils))
           (let ((bin (string-append (assoc-ref %outputs "out") "/bin"))
                 (python (string-append (assoc-ref %build-inputs "python") "/bin/")))
                (mkdir-p bin)
                (for-each
                  (lambda (old new)
                    (symlink (string-append python old)
                             (string-append bin "/" new)))
                  `("python3" ,"pydoc3" ,"pip3")
                  `("python"  ,"pydoc"  ,"pip"))
                ;; python-config outputs search paths based upon its location,
                ;; use a bash wrapper to avoid changing its outputs.
                (let ((bash (string-append (assoc-ref %build-inputs "bash")
                                           "/bin/bash"))
                      (old  (string-append python "python3-config"))
                      (new  (string-append bin "/python-config")))
                  (with-output-to-file new
                    (lambda ()
                      (format #t "#!~a~%" bash)
                      (format #t "exec \"~a\" \"$@\"~%" old)
                      (chmod new #o755))))))))
    (synopsis "Wrapper for the Python 3 commands")
    (description
     "This package provides wrappers for the commands of Python@tie{}3.x such
that they can also be invoked under their usual names---e.g., @command{python}
instead of @command{python3} or @command{pip} instead of @command{pip3}.

To function properly, this package should not be installed together with the
@code{python} package: this package uses the @code{python} package as a
propagated input, so installing this package already makes both the versioned
and the unversioned commands available.")))

(define-public python-wrapper (wrap-python3 python))
(define-public python-minimal-wrapper (wrap-python3 python-minimal))

(define-public python-pytz
  (package
    (name "python-pytz")
    ;; This package should be kept in sync with tzdata in (gnu packages base).
    (version "2022.1")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "pytz" version))
      (sha256
       (base32
        "19ya5sh7if819flgmszz585glailhi7rr8frng03n5m8wqphwxhy"))))
    (build-system python-build-system)
    (home-page "http://pythonhosted.org/pytz")
    (synopsis "Python timezone library")
    (description "This library brings the Olson tz database into Python.  It
allows accurate and cross platform timezone calculations using Python 2.4 or
higher.  It also solves the issue of ambiguous times at the end of daylight
saving time.  Almost all of the Olson timezones are supported.")
    (license expat)))

(define-public python-django
  (package
    (name "python-django")
    (version "1.11.29")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "Django" version))
              (sha256
               (base32
                "171jsi54fbnxzi2n3l4hkdmmwfnfrwacs180rw59l0bqcvxsw022"))))
    (build-system python-build-system)
    (arguments
     '(#:modules ((srfi srfi-1)
                  (guix packages python-build-system)
                  (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-tzdir
           (lambda* (#:key inputs #:allow-other-keys)
             ;; The test-suite tests timezone-dependent functions, thus tzdata
             ;; needs to be available.
             (setenv "TZDIR"
                     (string-append (assoc-ref inputs "tzdata")
                                    "/share/zoneinfo"))
             #t))
         (delete 'check)
         (delete 'sanity-check)
         (replace 'add-install-to-pythonpath
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (setenv "PYTHONPATH"
                     (string-append (or (assoc-ref outputs "python")
                                        (assoc-ref outputs "out"))
                                    "/lib/python" (python-version (assoc-ref inputs "python"))
                                    "/site-packages:"
                                    (getenv "PYTHONPATH")))))
         )))
    ;; TODO: Install extras/django_bash_completion.
    (native-inputs
     `(("tzdata" ,tzdata-for-tests)
       ;; Remaining packages are test requirements taken from
       ;; tests/requirements/py3.txt
       ("python-docutils" ,python-docutils)
       ;; optional for tests: ("python-geoip2" ,python-geoip2)
       ("python-jinja2" ,python-jinja2)           ; >= 2.7
       ;; optional for tests: ("python-memcached" ,python-memcached)
       ("python-numpy" ,python-numpy)
       ("python-pillow" ,python-pillow)
       ("python-pyyaml" ,python-pyyaml)
       ;; optional for tests: ("python-selenium" ,python-selenium)
       ("python-sqlparse" ,python-sqlparse)
       ("python-tblib" ,python-tblib)))
    (propagated-inputs
     `(("python-argon2-cffi" ,python-argon2-cffi)
       ("python-bcrypt" ,python-bcrypt)
       ("python-pytz" ,python-pytz)))
    (home-page "https://www.djangoproject.com/")
    (synopsis "High-level Python Web framework")
    (description
     "Django is a high-level Python Web framework that encourages rapid
development and clean, pragmatic design.  It provides many tools for building
any Web site.  Django focuses on automating as much as possible and adhering
to the @dfn{don't repeat yourself} (DRY) principle.")
    (license license:bsd-3)
    (properties `((cpe-name . "django")))))

(define-public python-onetimepass
  (package
    (name "python-onetimepass")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "onetimepass" version))
       (sha256
        (base32 "09vagxgbq78wvq4xbikmn2hpqqsa2i96kqsmphf7dqynfv0dlsd5"))))
    (build-system python-build-system)
    (propagated-inputs (list python-six))
    (home-page "https://github.com/tadeck/onetimepass/")
    (synopsis "One-time password library")
    (description "Python one-time password library for HMAC-based (HOTP) and
time-based (TOTP) passwords.")
    (license license:expat)))

(define-public python-ldap3
  (package
    (name "python-ldap3")
    (version "2.7")
    (home-page "https://github.com/cannatag/ldap3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference (url home-page)
                           (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xw9fkqld21xsvdpaqir8ccc2l805xnn9gxahsnl70xzp3mwl0xv"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f ;TODO: Tests need a real LDAP server to run
       #:phases (modify-phases %standard-phases
                  ;; (delete 'add-install-to-pythonpath)
                  ;; (add-after 'install 'add-install-to-pythonpath add-install-to-pythonpath)
                  (replace 'add-install-to-pythonpath
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (setenv "PYTHONPATH"
                              (string-append (assoc-ref outputs "out")
                                             "/lib/python3.6/site-packages:"
                                             (getenv "PYTHONPATH")))))
                  (delete 'sanity-check)
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests?
                        (invoke "nosetests" "-s" "test"))
                      #t)))))
    (native-inputs
     (list python-nose))
    (propagated-inputs
     (list python-gssapi python-pyasn1))
    (synopsis "Python LDAP client")
    (description
     "LDAP3 is a strictly RFC 4510 conforming LDAP V3 pure Python client
library.")
    (license license:lgpl3+)))

(define-public python-markdown
  (package
    (name "python-markdown")
    (version "3.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Markdown" version))
       (sha256
        (base32
         "0jbs73nincha8fkfxx267sfxac6pl0ckszjbqbb8gk4dhs8v9d9i"))))
    (build-system python-build-system)
    (native-inputs
     (list python-nose python-pyyaml))
    (home-page "https://python-markdown.github.io/")
    (synopsis "Python implementation of Markdown")
    (description
     "This package provides a Python implementation of John Gruber's
Markdown.  The library features international input, various Markdown
extensions, and several HTML output formats.  A command line wrapper
markdown_py is also provided to convert Markdown files to HTML.")
    (license license:bsd-3)))

(define-public python-markupsafe
  (package
    (name "python-markupsafe")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "MarkupSafe" version))
       (sha256
        (base32
         "0jqxp5sfrc0byp6bk0gwdmildi4mck2gprp42afri3z4r5y1k4bz"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before 'build 'move-sources
                    (lambda* (#:key inputs #:allow-other-keys)
                        (if (directory-exists? "src");(search-input-directory inputs "src"))
                            (copy-recursively "src";(search-input-directory inputs "src")
                                              ".");(search-input-directory inputs "."))
                            #t)))
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests?
                        (invoke "pytest" "-vv")))))))
    (native-inputs
     (list python-pytest))
    (home-page "https://github.com/mitsuhiko/markupsafe")
    (synopsis "XML/HTML/XHTML markup safe string implementation for Python")
    (description
     "Markupsafe provides an XML/HTML/XHTML markup safe string implementation
for Python.")
    (license license:bsd-3)))

(define-public python-mkdocs
  (package
    (name "python-mkdocs")
    (version "1.3.0")
    (source
     (origin
       ;; The tests suite appears to be incomplete in the PyPI archive.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mkdocs/mkdocs")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1n5rdllrxvhnxmdrddf55p3s86dakx0rq2gg6bj6pr6jg2pn932b"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Requirements refer to a specific version of dependencies,
         ;; which are too old. So we patch to refer to any later version.
         (add-after 'unpack 'patch-requirements
           (lambda _
             (substitute* "setup.py"
               (("==") ">="))))
         (delete 'check))))
    (propagated-inputs
     (list python-babel
           python-click
           python-ghp-import
           python-importlib-metadata
           python-jinja2
           python-markdown
           python-markupsafe
           python-mdx-gh-links
           python-mergedeep
           python-packaging
           python-pyyaml
           python-pyyaml-env-tag
           python-watchdog))
    (home-page "https://www.mkdocs.org")
    (synopsis "Project documentation with Markdown")
    (description "MkDocs is a static site generator geared towards building
project documentation.  Documentation source files are written in Markdown, and
configured with a single YAML configuration file.")
    (license license:bsd-3)))

(define-public python-openpyxl
  (package
    (name "python-openpyxl")
    (version "3.0.9")
    (source
     (origin
       ;; We use the upstream repository, as the tests are not included in the
       ;; PyPI releases.
       (method hg-fetch)
       (uri (hg-reference
             (url "https://foss.heptapod.net/openpyxl/openpyxl")
             (changeset version)))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32 "1p8xvc2gjw6zyzbd7qdvc3x178sm00ymrbyh9539l4fpzgxh0j9c"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check)
                    ;; (lambda _
                    ;;   (invoke "pytest")))
                  )))
    (native-inputs (list python-lxml python-pillow python-pytest))
    (propagated-inputs (list python-et-xmlfile python-jdcal))
    (home-page "https://openpyxl.readthedocs.io")
    (synopsis "Python library to read/write Excel 2010 XLSX/XLSM files")
    (description
     "This Python library allows reading and writing to the Excel XLSX, XLSM,
XLTX and XLTM file formats that are defined by the Office Open XML (OOXML)
standard.")
    (license license:expat)))

(define pycryptodome-unbundle-tomcrypt-snippet
  #~(begin
      ;; Unbundle libtomcrypt.
      (delete-file-recursively "src/libtom")
      (substitute* "src/DES.c"
        (("#include \"libtom/tomcrypt_des.c\"")
         "#include <tomcrypt.h>"))
      (substitute* "setup.py"
        (("include_dirs=\\['src/', 'src/libtom/'\\]")
         ;; FIXME: why does '-ltomcrypt' need to be added
         ;; manually, even when 'tomcrypt' is added to 'libraries'?
         ;; This behaviour is not documented at
         ;; <https://docs.python.org/3/extending/building.html>.
         "include_dirs=['src/'], libraries=['tomcrypt', 'tommath'],
 extra_link_args=['-ltomcrypt', '-ltommath']"))))

(define-public python-pycryptodome
  (package
    (name "python-pycryptodome")
    (version "3.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pycryptodome" version))
       (sha256
        (base32
         "1l3a80z3lxcj1q0hzj1d3plavy2d51y4vzcd85zj0zm7yyxrd022"))
       (modules '((guix build utils)))
       (snippet pycryptodome-unbundle-tomcrypt-snippet)))
    (build-system python-build-system)
    (inputs
     (list libtomcrypt libtommath))
    (home-page "https://www.pycryptodome.org")
    (synopsis "Low-level cryptographic Python library")
    (description
     "PyCryptodome is a self-contained Python package of low-level
cryptographic primitives.  It's not a wrapper to a separate C library like
OpenSSL.  To the largest possible extent, algorithms are implemented in pure
Python.  Only the pieces that are extremely critical to performance (e.g.,
block ciphers) are implemented as C extensions.

You are expected to have a solid understanding of cryptography and security
engineering to successfully use these primitives.  You must also be able to
recognize that some are obsolete (e.g., TDES) or even insecure (RC4).

It provides many enhancements over the last release of PyCrypto (2.6.1):

@itemize
@item Authenticated encryption modes (GCM, CCM, EAX, SIV, OCB)
@item Accelerated AES on Intel platforms via AES-NI
@item First-class support for PyPy
@item Elliptic curves cryptography (NIST P-256 curve only)
@item Better and more compact API (nonce and iv attributes for ciphers,
automatic generation of random nonces and IVs, simplified CTR cipher mode, and
more)
@item SHA-3 (including SHAKE XOFs) and BLAKE2 hash algorithms
@item Salsa20 and ChaCha20 stream ciphers
@item scrypt and HKDF
@item Deterministic (EC)DSA
@item Password-protected PKCS#8 key containers
@item Shamir’s Secret Sharing scheme
@item Random numbers get sourced directly from the OS (and not from a CSPRNG
in userspace)
@item Cleaner RSA and DSA key generation (largely based on FIPS 186-4)
@item Major clean-ups and simplification of the code base
@end itemize

This package provides drop-in compatibility with PyCrypto.  It is one of two
PyCryptodome variants, the other being python-pycryptodomex.")
    (license (list license:bsd-2
                   license:public-domain)))) ; code inherited from PyCrypto

(define-public python-pyyaml
  (package
    (name "python-pyyaml")
    (version "6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PyYAML" version))
       (sha256
        (base32
         "18imkjacvpxfgg1lbpraqywx3j7hr5dv99d242byqvrh2jf53yv8"))))
    (build-system python-build-system)
    (inputs
     (list libyaml python-cython))
    (home-page "https://pyyaml.org")
    (synopsis "YAML parser and emitter for Python")
    (description
     "PyYAML is a YAML parser and emitter for Python.  PyYAML features a
complete YAML 1.1 parser, Unicode support, pickle support, capable extension
API, and sensible error messages.  PyYAML supports standard YAML tags and
provides Python-specific tags that represent an arbitrary Python object.")
    (license license:expat)))

(define-public python-requests
  (package
    (name "python-requests")
    (version "2.27.1")
    (source (origin
             (method url-fetch)
             (uri (pypi-uri "requests" version))
             (sha256
              (base32
               "0qcsbi919d689xqlgyhw9zkppp1fs6k09wwffa3ri6d8smpwbmv8"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-certifi
           python-charset-normalizer
           python-idna
           python-urllib3))
    (arguments
     ;; FIXME: Some tests require network access.
     '(#:tests? #f))
    (home-page "http://python-requests.org/")
    (synopsis "Python HTTP library")
    (description
     "Requests is a Python HTTP client library.  It aims to be easier to use
than Python’s urllib2 library.")
    (license license:asl2.0)))

(define-public python-selenium
  (package
    (name "python-selenium")
    (version "3.141.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "selenium" version))
       (sha256
        (base32
         "039hf9knvl4s3hp21bzwsp1g5ri9gxsh504dp48lc6nr1av35byy"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-urllib3))
    (home-page
     "https://github.com/SeleniumHQ/selenium/")
    (synopsis "Python bindings for Selenium")
    (description "Selenium enables web browser automation.
Selenium specifically provides infrastructure for the W3C WebDriver specification
— a platform and language-neutral coding interface compatible with all
major web browsers.")
    (license license:asl2.0)))

(define-public python-six
  (package/inherit python-six-bootstrap
    (name "python-six")
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "pytest" "-v"))))))
    (native-inputs
     `(("python-pytest" ,python-pytest-bootstrap)))))

(define-public python-tornado
  (package
    (name "python-tornado")
    (version "5.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tornado" version))
       (sha256
        (base32
         "02clqk2116jbnq8lnaqmdw3p52nqrd9ib59r4xz2ll43fpcmhlaf"))))
    (build-system python-build-system)
    (arguments
     '(;; FIXME: Two tests error out with:
       ;; AssertionError: b'Error in atexit._run_exitfuncs:\nFileNotF[44 chars]ry\n' != b''
       ;; #:phases
       ;; (modify-phases %standard-phases
       ;;   (replace 'check
       ;;     (lambda _
       ;;       ;; 'setup.py test' hits an AssertionError on BSD-specific
       ;;       ;; "tornado/platform/kqueue.py". This is the supported method:
       ;;       (invoke "python" "-m" "tornado.test.runtests")
       ;;       #t)))
       #:tests? #f))
    (native-inputs
     (list python-certifi))
    (home-page "https://www.tornadoweb.org/")
    (synopsis "Python web framework and asynchronous networking library")
    (description
     "Tornado is a Python web framework and asynchronous networking library,
originally developed at FriendFeed.  By using non-blocking network I/O,
Tornado can scale to tens of thousands of open connections, making it ideal
for long polling, WebSockets, and other applications that require a long-lived
connection to each user.")
    (license license:asl2.0)))

(define-public python-tzlocal
  (package
    (name "python-tzlocal")
    (version "2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tzlocal" version))
       (sha256
        (base32
         "0i1fm4sl04y65qnaqki0w75j34w863gxjj8ag0vwgvaa572rfg34"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-symlink-test
           ;; see: https://github.com/regebro/tzlocal/issues/53
           (lambda _
             (delete-file "tests/test_data/symlink_localtime/etc/localtime")
             (symlink "../usr/share/zoneinfo/Africa/Harare"
                      "tests/test_data/symlink_localtime/etc/localtime")
             ;; And skip the test_fail test, it is known to fail
             (substitute* "tests/tests.py"
               (("def test_fail") "def _test_fail"))
             #t)))))
    (propagated-inputs
     (list python-pytz))
    (native-inputs
     (list python-mock))
    (home-page "https://github.com/regebro/tzlocal")
    (synopsis "Local timezone information for Python")
    (description
     "Tzlocal returns a tzinfo object with the local timezone information.
This module attempts to fix a glaring hole in pytz, that there is no way to
get the local timezone information, unless you know the zoneinfo name, and
under several distributions that's hard or impossible to figure out.")
    (license expat)))

(define-public python-utils
  (package
    (name "python-utils")
    (version "2.4.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "python-utils" version))
              (sha256
               (base32
                "12c0glzkm81ljgf6pwh0d4rmdm1r7vvgg3ifzp8yp9cfyngw07zj"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (delete-file "pytest.ini")
             (invoke "pytest" "-vv"))))))
    (native-inputs
     `(("pytest-runner" ,python-pytest-runner)
       ("pytest" ,python-pytest)
       ("six" ,python-six)))
    (home-page "https://github.com/WoLpH/python-utils")
    (synopsis "Convenient utilities not included with the standard Python install")
    (description
      "Python Utils is a collection of small Python functions and classes which
     make common patterns shorter and easier.")
    (license license:bsd-2)))

(define-public python-pymysql
  (package
    (name "python-pymysql")
    (version "0.9.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PyMySQL" version))
       (sha256
        (base32 "1ry8lxgdc1p3k7gbw20r405jqi5lvhi5wk83kxdbiv8xv3f5kh6q"))))
    (build-system python-build-system)
    (inputs
     (list python-cryptography))
    (arguments
     `(#:tests? #f))                    ; tests expect a running MySQL
    (home-page "https://github.com/PyMySQL/PyMySQL/")
    (synopsis "Pure-Python MySQL driver")
    (description
     "PyMySQL is a pure-Python MySQL client library, based on PEP 249.
Most public APIs are compatible with @command{mysqlclient} and MySQLdb.")
    (license license:expat)))

(define-public python-pysimplesoap
  (package
    (name "python-pysimplesoap")
    (version "1.16.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PySimpleSOAP" version))
       (sha256
        (base32 "1qb7dn8m1cjwzql7vqj9i1hsscb7nyhimmlp45jrpzxds38g9fxi"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (home-page "https://github.com/pysimplesoap/pysimplesoap/")
    (synopsis "Python Simple SOAP Library")
    (description
     "Python simple and lightweight SOAP library for client and server webservices interfaces,
      aimed to be as small and easy as possible, supporting most common functionality.
      Initially it was inspired by PHP Soap Extension (mimicking its functionality, simplicity
      and ease of use), with many advanced features added.")
    (license license:lgpl3+)))

(list python-3.6
      python-django
      python-six
      python-ldap3
      python-markdown
      python-onetimepass
      python-markupsafe
      python-mkdocs
      python-openpyxl
      python-pycryptodome
      python-pyyaml
      python-requests
      python-selenium
      python-tornado
      python-tzlocal
      python-pymysql
      python-pysimplesoap
      python-utils)
