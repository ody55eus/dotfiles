(define-module (python-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages check)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-compression)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages time)
  #:use-module (gnu packages xml))

(define-public python-cython
  (package
    (name "python-cython")
    (version "0.29.24")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Cython" version))
       (sha256
        (base32 "0hw4gs18rh4slij1fg252argxhraypld9apbqbl60230qc3lvw6d"))))
    (build-system python-build-system)
    ;; we need the full python package and not just the python-wrapper
    ;; because we need libpython3.3m.so
    (inputs
     (list python))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-HOME
           ;; some tests require access to "$HOME/.cython"
           (lambda _ (setenv "HOME" "/tmp")))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             ;; Disable compiler optimizations to greatly reduce the running
             ;; time of the test suite.
             (setenv "CFLAGS" "-O0")

             (when tests?
               (invoke "python" "runtests.py" "-vv"
                       "-j" (number->string (parallel-job-count))
                       ;; XXX: On 32-bit architectures, running the parallel tests
                       ;; fails on many-core systems, see
                       ;; <https://github.com/cython/cython/issues/2807>.
                       ,@(if (not (target-64bit?))
                             '("-x" "run.parallel")
                             '())
                       ;; This test fails when running on 24 cores.
                       "-x" "cpp_stl_conversion")))))))
    (home-page "https://cython.org/")
    (synopsis "C extensions for Python")
    (description "Cython is an optimising static compiler for both the Python
programming language and the extended Cython programming language.  It makes
writing C extensions for Python as easy as Python itself.")
    (license license:asl2.0)))

(define-public python-pyparsing
  (package
    (name "python-pyparsing")
    (version "3.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyparsing" version))
       (sha256
        (base32 "109b9r802wb472hgmxclljprh5cid0w3p6mk9alba7pg2c0frgfr"))))
    (build-system python-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:tests? #f                      ;no test target
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((doc (string-append (assoc-ref outputs "doc")
                                        "/share/doc/" ,name "-" ,version))
                    (html-doc (string-append doc "/html"))
                    (examples (string-append doc "/examples")))
               (mkdir-p html-doc)
               (mkdir-p examples)
               (for-each
                (lambda (dir tgt)
                  (map (lambda (file)
                         (install-file file tgt))
                       (find-files dir ".*")))
                (list "docs" "htmldoc" "examples")
                (list doc html-doc examples))))))))
    (home-page "https://github.com/pyparsing/pyparsing")
    (synopsis "Python parsing class library")
    (description
     "The pyparsing module is an alternative approach to creating and
executing simple grammars, vs. the traditional lex/yacc approach, or the use
of regular expressions.  The pyparsing module provides a library of classes
that client code uses to construct the grammar directly in Python code.")
    (license license:expat)))

(define-public python-six-bootstrap
  (package
    (name "python-six-bootstrap")
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "six" version))
       (sha256
        (base32
         "09n9qih9rpj95q3r4a40li7hk6swma11syvgwdc68qm1fxsc6q8y"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f))          ;to avoid pytest dependency
    (home-page "https://pypi.org/project/six/")
    (synopsis "Python 2 and 3 compatibility utilities")
    (description
     "Six is a Python 2 and 3 compatibility library.  It provides utility
functions for smoothing over the differences between the Python versions with
the goal of writing Python code that is compatible on both Python versions.
Six supports every Python version since 2.5.  It is contained in only one
Python file, so it can be easily copied into your project.")
    (license license:x11)))

(define-public python-packaging-bootstrap
  (package
    (name "python-packaging-bootstrap")
    (version "21.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "packaging" version))
       (sha256
        (base32
         "1sygirdrqgv4f1ckh9nhpcw1yfidrh3qjl86wq8vk6nq4wlw8iyx"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f))         ;disabled to avoid extra dependencies
    (propagated-inputs
     (list python-pyparsing python-six-bootstrap))
    (home-page "https://github.com/pypa/packaging")
    (synopsis "Core utilities for Python packages")
    (description "Packaging is a Python module for dealing with Python packages.
It offers an interface for working with package versions, names, and dependency
information.")
    ;; From 'LICENSE': This software is made available under the terms of
    ;; *either* of the licenses found in LICENSE.APACHE or LICENSE.BSD.
    ;; Contributions to this software is made under the terms of *both* these
    ;; licenses.
    (license (list license:asl2.0 license:bsd-2))))

(define-public python-setuptools-scm
  (package
    (name "python-setuptools-scm")
    (version "6.3.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "setuptools_scm" version))
              (sha256
               (base32 "1wm0i27siyy1yqr9rv7lqvb65agay9051yi8jzmi8dgb3q4ai6m4"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-packaging",python-packaging-bootstrap)
       ("python-tomli" ,python-tomli)))
    (home-page "https://github.com/pypa/setuptools_scm/")
    (synopsis "Manage Python package versions in SCM metadata")
    (description
     "Setuptools_scm handles managing your Python package versions in
@dfn{software configuration management} (SCM) metadata instead of declaring
them as the version argument or in a SCM managed file.")
    (license license:expat)))

(define-public python-pytest
  (package
    (name "python-pytest")
    (version "6.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest" version))
       (sha256
        (base32
         "12cyi0lnyaq8sdqfnqlppd76gkw6zcg10gyih5knx9v611l3c6qk"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'pretend-version
           ;; The version string is usually derived via setuptools-scm, but
           ;; without the git metadata available, the version string is set to
           ;; '0.0.0'.
           (lambda _
             (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" ,version)))
         (replace 'check
           (lambda* (#:key (tests? #t) #:allow-other-keys)
             (setenv "TERM" "dumb")     ;attempt disabling markup tests
             (if tests?
                 (invoke "pytest" "-vv" "-k"
                         (string-append
                          ;; This test involves the /usr directory, and fails.
                          " not test_argcomplete"
                          ;; These test do not honor the isatty detection and
                          ;; fail.
                          " and not test_code_highlight"
                          " and not test_color_yes"))
                 (format #t "test suite not run~%")))))))
    (propagated-inputs
     `(("python-attrs" ,python-attrs-bootstrap)
       ("python-iniconfig" ,python-iniconfig)
       ("python-more-itertools" ,python-more-itertools)
       ("python-packaging" ,python-packaging-bootstrap)
       ("python-pluggy" ,python-pluggy)
       ("python-py" ,python-py)
       ("python-six" ,python-six-bootstrap)
       ("python-toml" ,python-toml)
       ("python-wcwidth" ,python-wcwidth)))
    (native-inputs
     `(;; Tests need the "regular" bash since 'bash-final' lacks `compgen`.
       ("bash" ,bash)
       ("python-hypothesis" ,python-hypothesis)
       ("python-nose" ,python-nose)
       ("python-mock" ,python-mock)
       ("python-pytest" ,python-pytest-bootstrap)
       ("python-setuptools-scm" ,python-setuptools-scm)
       ("python-toml" ,python-toml)
       ("python-xmlschema" ,python-xmlschema)))
    (home-page "https://docs.pytest.org/en/latest/")
    (synopsis "Python testing library")
    (description
     "Pytest is a testing tool that provides auto-discovery of test modules
and functions, detailed info on failing assert statements, modular fixtures,
and many external plugins.")
    (license license:expat)))

(define-public python-pytest-xdist
  (package
    (name "python-pytest-xdist")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-xdist" version))
       (sha256
        (base32
         "0wh6pn66nncfs6ay0n863bgyriwsgppn8flx5l7551j1lbqkinc2"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove pre-compiled .pyc files from source.
           (for-each delete-file-recursively
                     (find-files "." "__pycache__" #:directories? #t))
           (for-each delete-file (find-files "." "\\.pyc$"))
           #t))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f ; Lots of tests fail.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-setup-py
           (lambda _
             ;; Relax pytest requirement.
             (substitute* "setup.py"
               (("pytest>=6\\.0\\.0") "pytest"))))
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "pytest" "-vv"
                       "-n" (number->string (parallel-job-count)))))))))
    (native-inputs
     (list python-setuptools-scm))
    (propagated-inputs
     (list python-execnet python-pytest python-py python-pytest-forked))
    (home-page
     "https://github.com/pytest-dev/pytest-xdist")
    (synopsis
     "Plugin for py.test with distributed testing and loop-on-failing modes")
    (description
     "The pytest-xdist plugin extends py.test with some unique test execution
modes: parallelization, running tests in boxed subprocesses, the ability
to run tests repeatedly when failed, and the ability to run tests on multiple
Python interpreters or platforms.  It uses rsync to copy the existing
program code to a remote location, executes there, and then syncs the
result back.")
    (license license:expat)))

(define-public python-numpy
(package
    (name "python-numpy")
    (version "1.21.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/numpy/numpy/releases/download/v"
             version "/numpy-" version ".tar.gz"))
       (sha256
        (base32
         "0b0c5y35rd3mvwfk5is1d5ppfw9nl4d2rgx9xkwh1p0w394wdvyl"))))
    (build-system python-build-system)
    (arguments
     (list
      #:modules '((guix build utils)
                  (guix build python-build-system)
                  (ice-9 format))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'parallelize-build
            (lambda _
              (setenv "NPY_NUM_BUILD_JOBS"
                      (number->string (parallel-job-count)))))
          (add-before 'build 'configure-blas
            (lambda* (#:key inputs #:allow-other-keys)
              (call-with-output-file "site.cfg"
                (lambda (port)
                  (format port
                          "\
[openblas]
libraries = openblas
library_dirs = ~a/lib
include_dirs = ~:*~a/include~%" #$(this-package-input "openblas"))))))
          (add-before 'build 'fix-executable-paths
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Make /gnu/store/...-bash-.../bin/sh the default shell,
              ;; instead of /bin/sh.
              (substitute* "numpy/distutils/exec_command.py"
                (("'/bin/sh'")
                 (format #f "~s" (search-input-file inputs "bin/bash"))))
              ;; Don't try to call '/bin/true' specifically.
              (substitute* "numpy/core/tests/test_cpu_features.py"
                (("/bin/true") (search-input-file inputs "bin/true")))))
          (delete 'check))))
    (native-inputs
     (list python-cython
           python-hypothesis-next
           python-pytest
           python-pytest-xdist
           gfortran))
    (inputs (list bash openblas))
    (home-page "https://numpy.org")
    (synopsis "Fundamental package for scientific computing with Python")
    (description "NumPy is the fundamental package for scientific computing
with Python.  It contains among other things: a powerful N-dimensional array
object, sophisticated (broadcasting) functions, tools for integrating C/C++
and Fortran code, useful linear algebra, Fourier transform, and random number
capabilities.")
    (properties
     '((upstream-name . "numpy")))
    (license license:bsd-3)))
