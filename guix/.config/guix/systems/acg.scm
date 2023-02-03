;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules (gnu)
             (nongnu packages linux))
(use-package-modules
 xdisorg
 fonts
 gnome)
(use-service-modules
 admin
 databases
 desktop
 networking
 ssh
 docker
 nix
 virtualization
 xorg)

(define %my-base-services
  (append
   (cons*
    (screen-locker-service xlockmore "xlock")
    (service nix-service-type)
    (service docker-service-type)
    (service unattended-upgrade-service-type
             (unattended-upgrade-configuration
              (schedule "30 01 * * 0")
              (operating-system-file
               (file-append
                (local-file (string-append (getenv "HOME")
                                           "/.dotfiles/guix/.config/guix/systems")
                            "guix-systems"
                            #:recursive? #t)
                "/acg.scm"))))
    (service openssh-service-type)
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
  (kernel linux)
  (firmware (list linux-firmware))
  (locale "en_GB.utf8")
  (timezone "Europe/Berlin")
  (keyboard-layout (keyboard-layout "de" "neo"))
  (host-name "ACG-JPI-NonGuix")
  (users (cons* (user-account
                  (name "jpi")
                  (comment "Jonathan Pieper")
                  (group "users")
                  (home-directory "/home/jpi")
                  (supplementary-groups
                    '("wheel" "netdev" "audio" "video")))
                (user-account
                  (name "jp")
                  (comment "Jonathan Pieper")
                  (group "users")
                  (home-directory "/home/jp")
                  (supplementary-groups
                    '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))
  (packages (append (map specification->package
                         "awesome"
                         "nss-certs")
      %base-packages))
  (services
   (append
    (list
     (service slim-service-type (slim-configuration
                                 (display ":0")
                                 (vt "vt7")
                                 (gnupg? #t)
                                 (allow-empty-passwords? #t)
                                 (auto-login? #t)
                                 (default-user "jpi")
                                 (xorg-configuration
                                  (xorg-configuration
                                   (keyboard-layout keyboard-layout)))
                                 ))
     (service mysql-service-type))
    %my-base-services))
  (bootloader
   (bootloader-configuration
    (bootloader grub-bootloader)
    (targets (list "/dev/sda"))
    (keyboard-layout keyboard-layout)))
  (swap-devices
   (list (swap-space
          (target
           (uuid "aa4b5be5-e260-45bf-8bdd-c9df6478e963")))))
  (file-systems
   (cons* (file-system
            (mount-point "/")
            (device
             (uuid "0cce4a5f-deda-4877-b628-4188d944dbbb"
                   'ext4))
            (type "ext4"))
          %base-file-systems)))
