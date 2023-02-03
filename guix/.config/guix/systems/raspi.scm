(use-modules (gnu)
             (gnu artwork)
             (gnu system nss))
(use-service-modules admin
                     avahi
                     desktop
                     xorg
                     networking
                     ssh)
(use-package-modules certs
                     linux
                     raspberry-pi
                     fonts
                     shells
                     wm
                     ssh)

(define-public raspberry-pi-64
  (operating-system
    (host-name "raspberrypi-guix")
    (locale "en_GB.utf8")
    (keyboard-layout (keyboard-layout "de" "neo"))
    (timezone "Europe/Berlin")
    (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader-chain-raspi-64)
                 (targets (list "/boot/efi"))
                 (theme (grub-theme
                         (resolution '(1920 . 1080))
                         (image (file-append
                                 %artwork-repository
                                 "/grub/GuixSD-fully-black-16-9.svg"))))))
    (kernel (customize-linux #:linux linux-libre-arm64-generic
                             ;; It is possible to use a specific defconfig
                             ;; file, for example the "bcmrpi3_defconfig" with
                             ;; the variable shown below.  Unfortunately the
                             ;; kernel built from the linux-libre sources with
                             ;; this defconfig file does not boot.
                             ;;#:extra-version "gnu-bcmrpi3"
                             ;;#:defconfig %bcmrpi3-defconfig
                             ))
    (initrd-modules '())
    (file-systems (cons* (file-system
                           (mount-point "/")
                           (type "ext4")
                           (device (file-system-label "Guix")))
                         (file-system
                           (mount-point "/boot/efi")
                           (type "vfat")
                           (device (file-system-label "EFI")))
                         %base-file-systems))
    (swap-devices (list (swap-space
                         (target "/run/swapfile"))))
    (users (cons* (user-account
                   (name "pi")
                   (group "users")
                   (supplementary-groups '("wheel" "netdev" "audio" "video"))
                   (home-directory "/home/pi"))
                  (user-account
                   (name "jp")
                   (group "users")
                   (supplementary-groups '("wheel" "netdev" "audio" "video"))
                   (home-directory "/home/jp"))
                  %base-user-accounts))
    (packages (cons* nss-certs
                     openssh
                     zsh
                     sway
                     qtile
                     %base-packages))
    (services (cons*
               ;; (service slim-service-type (slim-configuration
               ;;                             (display ":0")
               ;;                             (vt "vt7")
               ;;                             (gnupg? #t)
               ;;                             (allow-empty-passwords? #t)
               ;;                             (auto-login? #t)
               ;;                             (default-user "jp")
               ;;                             (xorg-configuration
               ;;                              (xorg-configuration
               ;;                               (keyboard-layout keyboard-layout)))
               ;;                             ))
               (service avahi-service-type)
               (service dhcp-client-service-type)
               (service ntp-service-type)
               (extra-special-file "/usr/bin/env"
                                   (file-append coreutils "/bin/env"))
               (extra-special-file "/usr/bin/zsh"
                                   (file-append zsh "/bin/zsh"))
               (service openssh-service-type
                        (openssh-configuration
                         (x11-forwarding? #t)))
               (modify-services %base-services;; %desktop-services
                 ;; (delete gdm-service-type)
                 (console-font-service-type
                  config =>
                  `(("tty1" . "LatGrkCyr-8x16")
                    ("tty2" . ,(file-append
                                font-tamzen
                                "/share/kbd/consolefonts/TamzenForPowerline10x20.psf"))
                    ("tty3" . ,(file-append
                                font-terminus
                                "/share/consolefonts/ter-132n")) ; for HDPI
                    ("tty4" . ,(file-append
                                font-terminus
                                "/share/consolefonts/ter-132n"))
                    ("tty5" . ,(file-append
                                font-terminus
                                "/share/consolefonts/ter-132n"))
                    ("tty6" . ,(file-append
                                font-terminus
                                "/share/consolefonts/ter-132n")))))))
    (name-service-switch %mdns-host-lookup-nss)))

raspberry-pi-64
