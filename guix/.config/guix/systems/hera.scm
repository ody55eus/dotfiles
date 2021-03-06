(use-modules (gnu)
             (gnu system nss)
             (srfi srfi-1)
             (guix channels)
             (guix git-download)
             (guix inferior))
;; (use-modules (nongnu packages linux))
;; (use-modules (nongnu system linux-initrd))

(use-service-modules
 cups
 desktop
 networking
 sound
 ssh
 docker
 nix
 virtualization
 xorg)
(use-package-modules gnome fonts)

(define %my-base-services
  (append (list
   (service openssh-service-type)
   (service cups-service-type))
   (modify-services %desktop-services
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
 ;;    ))
 ;; Non-Free Kernel
 ;; (kernel linux)
 ;;(initrd microcode-initrd)
 ;;(firmware (list linux-firmware))
 (locale "en_US.utf8")
 (timezone "Europe/Berlin")
 (keyboard-layout (keyboard-layout "de" "neo"))
 (packages (append (map specification->package
                        '(
                          
                          "alsa-utils"
                          "pavucontrol"
                          
                          "system-config-printer"
                          
                          "font-ibm-plex"        ;; The fonts have been designed to work well in user interface (UI) environments as well as other mediums.
                          "font-overpass"        ;; Overpass is a sans-serif typeface based on the U.S.  interstate highway road signage typefaces
                          "font-juliamono"       ;; JuliaMono is a monospaced font for scientific and technical computing
                          "font-jetbrains-mono"  ;; JetBrains Mono is a font family dedicated to developers
                          
                          "alacritty"
                          "direnv"
                          "zsh"
                          "tmux"
                          "openssh"
                          "git"
                          "pinentry"
                          "bat"
                          "zip"
                          "unzip"
                          "ripgrep"
                          "the-silver-searcher" ; ag
                          "trash-cli"
                          "glibc-locales"
                          "nss-certs"
                          
                          "awesome"
                          
                          "xev"
                          "xset"
                          "xrdb"
                          "xhost"
                          "xmodmap"
                          "setxkbmap"
                          "xrandr"
                          "arandr"
                          "xss-lock"
                          "libinput"
                          "xinput"
                          
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
 

 (host-name "hera")

(services (append
           (list
            (service nix-service-type)
            (service docker-service-type)
            (set-xorg-configuration
             (xorg-configuration
              (keyboard-layout keyboard-layout))))
           %my-base-services))

 (bootloader
  (bootloader-configuration
   (bootloader grub-bootloader)
   (targets (list "/dev/sdb"))
   (keyboard-layout keyboard-layout)))

 (swap-devices
  (list (swap-space
         (target (uuid "b6879df0-45fc-49cb-a091-a64fdbba2115")))))

 (file-systems
  (cons* (file-system
          (mount-point "/")
          (device
           (uuid "c2051dff-a06b-42f9-aed4-86383c41db69"
                 'ext4))
          (type "ext4"))
         %base-file-systems)))
