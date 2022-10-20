(define-module (python-django)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix packages python)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages check)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-compression)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages time)
  #:use-module (gnu packages xml))

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
                  (guix build python-build-system)
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
         (delete 'check))))
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

python-django
