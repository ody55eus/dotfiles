(use-modules ((guix licenses) #:prefix license:))
(use-modules (gnu packages))
(use-modules (gnu packages algebra))
(use-modules (gnu packages adns))
(use-modules (gnu packages attr))
(use-modules (gnu packages backup))
(use-modules (gnu packages bash))
(use-modules (gnu packages check))
(use-modules (gnu packages compression))
(use-modules (gnu packages crypto))
(use-modules (gnu packages databases))
(use-modules (gnu packages file))
(use-modules (gnu packages fontutils))
(use-modules (gnu packages gcc))
(use-modules (gnu packages geo))
(use-modules (gnu packages ghostscript))
(use-modules (gnu packages gl))
(use-modules (gnu packages glib))
(use-modules (gnu packages graphviz))
(use-modules (gnu packages graphics))
(use-modules (gnu packages gstreamer))
(use-modules (gnu packages gtk))
(use-modules (gnu packages icu4c))
(use-modules (gnu packages image))
(use-modules (gnu packages imagemagick))
(use-modules (gnu packages libevent))
(use-modules (gnu packages libffi))
(use-modules (gnu packages linux))
(use-modules (gnu packages machine-learning))
(use-modules (gnu packages man))
(use-modules (gnu packages maths))
(use-modules (gnu packages multiprecision))
(use-modules (gnu packages networking))
(use-modules (gnu packages ncurses))
(use-modules (gnu packages openstack))
(use-modules (gnu packages pcre))
(use-modules (gnu packages perl))
(use-modules (gnu packages pkg-config))
(use-modules (gnu packages python-crypto))
(use-modules (gnu packages python-web))
(use-modules (gnu packages qt))
(use-modules (gnu packages readline))
(use-modules (gnu packages sdl))
(use-modules (gnu packages search))
(use-modules (gnu packages shells))
(use-modules (gnu packages ssh))
(use-modules (gnu packages statistics))
(use-modules (gnu packages terminals))
(use-modules (gnu packages tex))
(use-modules (gnu packages texinfo))
(use-modules (gnu packages time))
(use-modules (gnu packages tls))
(use-modules (gnu packages version-control))
(use-modules (gnu packages video))
(use-modules (gnu packages web))
(use-modules (gnu packages base))
(use-modules (gnu packages xml))
(use-modules (gnu packages xorg))
(use-modules (gnu packages xdisorg))
(use-modules (gnu packages tcl))
(use-modules (gnu packages bdw-gc))
(use-modules (guix packages))
(use-modules (guix download))
(use-modules (guix git-download))
(use-modules (guix utils))
(use-modules (guix build-system gnu))
(use-modules (guix build-system cmake))
(use-modules (guix build-system python))
(use-modules (guix build-system trivial))
(use-modules (srfi srfi-1))

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
                 #t)))))))
    (native-search-paths
     (list (search-path-specification
            (variable "PYTHONPATH")
            (files (list (string-append "lib/python"
                                        (version-major+minor version)
                                        "/site-packages"))))))))
