(use-modules (gnu)
             (gnu system nss)
             (srfi srfi-1)
             (guix channels)
             (guix git-download)
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
 linux
 fonts)

(define %my-base-services
  (append (list
            (screen-locker-service xlockmore "xlock")
            (service unattended-upgrade-service-type
                     (unattended-upgrade-configuration
                       (schedule "30 01 * * 0")
                       (operating-system-file
                         (file-append
                           (local-file (string-append (getenv "HOME")
                                                      "/.dotfiles/guix/.config/guix/systems")
                                       "guix-systems"
                                       #:recursive? #t)
                           "/circe.scm"))))
            (service openssh-service-type)
            (service cups-service-type))
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
                                            "/share/consolefonts/ter-132n")))))))


(operating-system
  ;; (kernel
  ;;  (let*
  ;;      ((channels
  ;;        (list (channel
  ;;               (name 'nonguix)
  ;;               (url "https://gitlab.com/nonguix/nonguix")
  ;;               (commit "393b8e0405f44835c498d7735a8ae9ff4682b07f"))
  ;;              (channel
  ;;               (name 'guix)
  ;;               (url "https://git.savannah.gnu.org/git/guix.git")
  ;;               (commit "4c812db049d5c9f2c438748e180f9486ad221b0a"))))
  ;;       (inferior
  ;;        (inferior-for-channels channels)))
  ;;     (first (lookup-inferior-packages inferior "linux" "5.15.12")) ;; Pinning Kernel Version
  ;;    )
  ;; Non-Free Kernel
  (kernel linux-libre)
  (initrd microcode-initrd)
  (firmware (list linux-firmware))
  (locale "en_US.utf8")
  (timezone "Europe/Berlin")
  (keyboard-layout (keyboard-layout "de" "neo"))
  (packages (append (map specification->package
                         '(
                           ; UI
                           "awesome"
                           "alacritty-next"

                           ; Mounting
                           "gvfs"
                           "ntfs-3g"
                           "exfat-utils"
                           "fuse-exfat"
                           
                           ; Bluetooth
                           "bluez"
                           "bluez-alsa"

                           ; Terminal
                           "openssh"
                           "zsh"
                           "neovim"
                           "htop"

                           ; base-packages
                           "nss-certs"   ; Certificates
                           "zstd"        ; Compression
                           "wget" "curl" ; Downloading
                           "git"
                           ))
                    %base-packages))
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
                  (name "private")
                  (comment "Jonathan Pieper Privat")
                  (group "users")
                  (home-directory "/home/priv")
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
                  (name "guest")
                  (comment "Gast")
                  (group "users")
                  (home-directory "/home/guest")
                  (supplementary-groups
                    '(
                      "netdev" ;; network devices
                      "audio"
                      "video"
                      "input"
                      "tty"
                      "scanner"
                      "lp")))
                %base-user-accounts))
  (host-name "circe")
  (services (append
              (list
                (service nix-service-type)
                (service docker-service-type)
                (service slim-service-type (slim-configuration
                                             (display ":0")
                                             (vt "vt7")
                                             (gnupg? #t)
                                             (allow-empty-passwords? #f)
                                             (auto-login? #f)
                                             (default-user "jp")
                                             (xorg-configuration
                                               (xorg-configuration
                                                 (keyboard-layout keyboard-layout)))
                                             ))
                )
              %my-base-services))
  (bootloader
    (bootloader-configuration
      (bootloader grub-bootloader)
      (targets (list "/dev/sda"))
      (keyboard-layout keyboard-layout)))
  (swap-devices
    (list (swap-space
            (target (uuid "247722ef-5a54-419f-a810-6ee2d160ac9d")))))
  (file-systems
    (cons* (file-system
             (mount-point "/")
             (device
               (uuid "ccfdb723-4ce7-4edd-a2f1-8518af456e1f"
                     'ext4))
             (type "ext4"))
           (file-system
            (mount-point "/mnt/")
            (device "/dev/sdc1")
            (type "btrfs"))
           %base-file-systems)))
