(define-module (wazuh)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages search)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages base)
  #:use-module (gnu packages xml)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (srfi srfi-1))

(define-public wazuh-agent
  (package
    (name "wazuh-agent")
    (version "4.3.8")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/wazuh/wazuh/archive/refs/tags/v"
                    version ".tar.xz"))
              (sha256
               (base32
                "19l7inxm056jjw33zz97z0m02hsi7jnnx5kyb76abj5ml4xhad7l"))))
    (build-system gnu-build-system) ;or cmake-build-system
    (inputs (list curl
                  gcc
                  gcc-toolchain
                  make
                  wget
                  expect
                  gnupg
                  perl-test-base
                  perl
                  fakeroot
                  brotli
                  automake
                  autoconf
                  libtool
                  go
                  gawk
                  libsigsegv
                  inetutils
                  cmake
                  node
                  openssl))
    (native-inputs (list pkg-config python))
    (arguments
     '(#:tests? #f
       #:configure-flags (let ((out (assoc-ref %outputs "out")))
                           (list "-DCC=gcc"
                                 (string-append
                                  "-DCMAKE_INSTALL_SYSCONF_PREFIX=" out "/etc")
                                 (string-append "-DBASHCOMPLETIONDIR=" out
                                                "/etc/bash_completion.d")))
       ;; #:phases
       ;; (modify-phases %standard-phases
       ;; )
       ))
    (synopsis "Wazuh SIEM Agent")
    (description "Wazuh Agent collects and sends logfiles to the Wazuh Server")
    (home-page "https://wazuh.com")
    (license (list license:gpl2 license:openssl))))
