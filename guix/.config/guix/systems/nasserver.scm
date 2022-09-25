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
        cups
        desktop
        networking
        ssh
        xorg)

(define %my-base-services
  (append
   (cons*
    (service openssh-service-type)
    (extra-special-file "/usr/bin/env"
     (file-append coreutils "/bin/env"))
    (service cups-service-type)
    (modify-services %desktop-services
                     (network-manager-service-type
                      config =>
                      (network-manager-configuration
                       (inherit config)
                       (vpn-plugins (list network-manager-openvpn))))))))

(operating-system
  ; (kernel
  ;  (let*
  ;      ((channels
  ;        (list (channel
  ;               (name 'nonguix)
  ;               (url "https://gitlab.com/nonguix/nonguix")
  ;               (commit "e0951349603581895e0ba61f0e7410368ea1902a"))
  ;              (channel
  ;               (name 'guix)
  ;               (url "https://git.savannah.gnu.org/git/guix.git")
  ;               (commit "ca94157380b4ceb92f940f1d95c44e79b4521874"))))
  ;       (inferior
  ;        (inferior-for-channels channels)))
  ;     (first (lookup-inferior-packages inferior "linux" "5.19.8")) ;; Pinning Kernel Version
  ;    ))
  ;; Non-Free Kernel
 (kernel linux)
 (initrd microcode-initrd)
 (firmware (list linux-firmware))
 (locale "en_US.utf8")
 (timezone "Europe/Berlin")
 (keyboard-layout (keyboard-layout "de" "neo"))
 (packages
  (append
      (list (specification->package "awesome")
            (specification->package "bat")
            (specification->package "neovim")
            (specification->package "alsa-utils")
            (specification->package "pavucontrol")
            (specification->package "system-config-printer")
            (specification->package "font-ibm-plex")        ;; The fonts have been designed to work well in user interface (UI) environments as well as other mediums.
            (specification->package "font-overpass")        ;; Overpass is a sans-serif typeface based on the U.S.  interstate highway road signage typefaces
            (specification->package "font-juliamono")       ;; JuliaMono is a monospaced font for scientific and technical computing
            (specification->package "font-jetbrains-mono")  ;; JetBrains Mono is a font family dedicated to developers
            (specification->package "emacs-next-pgtk-latest")
            (specification->package "direnv")
            (specification->package "zsh")
            (specification->package "tmux")
            (specification->package "openssh")
            (specification->package "git")
            (specification->package "bat")
            (specification->package "zip")
            (specification->package "unzip")
            (specification->package "ripgrep")
            (specification->package "the-silver-searcher") ; ag
            (specification->package "trash-cli")
            (specification->package "glibc-locales")
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
            (set-xorg-configuration
             (xorg-configuration
              (keyboard-layout keyboard-layout)))
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
