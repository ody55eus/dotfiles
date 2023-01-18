(use-modules (gnu)
             (gnu system nss)
             (srfi srfi-1)
             (guix channels)
             (guix git-download)
             (gnu packages gnome)
             (guix inferior))
(use-modules (nongnu packages linux))
(use-modules (nongnu system linux-initrd))

(use-service-modules
 admin
 cups
 desktop
 networking
 sound
 ssh
 docker
 nix
 virtualization
 xorg)
(use-package-modules
 xdisorg
 gnome
 fonts)

(define %my-base-services
  (append
   (cons*
    (screen-locker-service xlockmore "xlock")
    (service guix-publish-service-type)
    (service unattended-upgrade-service-type
             (unattended-upgrade-configuration
              (schedule "30 01 * * 0")
              (operating-system-file
               (file-append
                (local-file (string-append (getenv "HOME")
                                           "/.dotfiles/guix/.config/guix/systems")
                            "guix-systems"
                            #:recursive? #t)
                "/hera.scm"))))
    (service openssh-service-type)
    (extra-special-file "/usr/bin/env"
                        (file-append coreutils "/bin/env"))
    (service cups-service-type)
    (modify-services %desktop-services
                     (delete gdm-service-type)
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
                                    "/share/consolefonts/ter-132n"))))
                     (network-manager-service-type
                      config =>
                      (network-manager-configuration
                       (inherit config)
                       (vpn-plugins (list network-manager-openvpn))))))))

(operating-system
  ;; Non-Free Kernel
 (kernel linux)
 (initrd microcode-initrd)
 (firmware (list linux-firmware))
 (locale "en_US.utf8")
 (timezone "Europe/Berlin")
 (keyboard-layout (keyboard-layout "de" "neo"))
 (packages
  (append
      (list ;; (specification->package "awesome")
            (specification->package "nss-certs"))
   %base-packages))
 (host-name "nasserver")
 (users (cons* (user-account
                (name "jp")
                (comment "Jonathan Pieper")
                (group "users")
                (home-directory "/home/jp")
                (supplementary-groups
                 '("wheel"  ;; sudo
                   "netdev" ;; network devices
                   "audio"
                   "video"
                   "input"
                   "tty"
                   "scanner"
                   "lp")))
               (user-account
                (name "bkp")
                (comment "Backup User")
                (group "users")
                (home-directory "/home/bkp")
                (supplementary-groups
                 '(
                   "netdev" ;; network devices
                   )))
               %base-user-accounts))
 (services (cons*
            ;; (set-xorg-configuration
            ;;  (xorg-configuration
            ;;   (keyboard-layout keyboard-layout)))
            %my-base-services))
 (bootloader
  (bootloader-configuration
   (bootloader grub-bootloader)
   (targets (list "/dev/sda"))
   (keyboard-layout keyboard-layout)))
 (swap-devices
  (list (swap-space
         (target
          (uuid "5e3f3adf-c169-4e92-8265-2366f5b0aa3f")))))
 (file-systems
  (cons* (file-system
          (mount-point "/")
          (device
           (uuid "24936c30-c01d-4fe9-9160-9b2b11e9db0f"
                 'ext4))
          (type "ext4"))
         (file-system
          (mount-point "/mnt/")
          (device "/dev/sdb1")
          (type "ext4"))
         %base-file-systems)))
