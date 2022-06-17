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
        ssh
        xorg)
(define %my-base-services
  (append
   (cons*
    (service openssh-service-type)
    (service cups-service-type)
    (modify-services %desktop-services
                     (network-manager-service-type
                      config =>
                      (network-manager-configuration
                       (inherit config)
                       (vpn-plugins (list network-manager-openvpn))))))))


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
 (packages
  (append
   (specifications->manifest
    '(
      
      "compton"
      "redshift"
      "gucharmap"
      "fontmanager"
      "brightnessctl"
      "xdg-utils"      ;; For xdg-open, etc
      "xdg-dbus-proxy" ;; For Flatpak
      "gtk+:bin"       ;; For gtk-launch
      "glib:bin"       ;; For gio-launch-desktop
      "shared-mime-info"
      
      "icecat"
      "nyxt"
      
      "alsa-utils"
      "pavucontrol"
      
      "vlc"
      
      "mpv"
      "youtube-dl"
      "playerctl"
      
      "gstreamer"
      "gst-plugins-base"
      "gst-plugins-good"
      "gst-plugins-bad"
      "gst-plugins-ugly"
      "gst-libav"
      "intel-vaapi-driver"
      "libva-utils"
      
      "feh"
      "gimp"
      "scrot"
      
      "zathura"
      "zathura-pdf-mupdf"
      
      "flatpak"
      
      "system-config-printer"
      
      "font-ibm-plex"        ;; The fonts have been designed to work well in user interface (UI) environments as well as other mediums.
      "font-overpass"        ;; Overpass is a sans-serif typeface based on the U.S.  interstate highway road signage typefaces
      "font-juliamono"       ;; JuliaMono is a monospaced font for scientific and technical computing
      "font-jetbrains-mono"  ;; JetBrains Mono is a font family dedicated to developers
      
      "texlive"
      "pandoc"
      
      "isync"
      "mu"
      
      "emacs-next-pgtk-latest"
      
      "alacritty"
      "direnv"
      "zsh"
      "tmux"
      "openssh"
      "git"
      "bat"
      "zip"
      "unzip"
      "ripgrep"
      "the-silver-searcher" ; ag
      "trash-cli"
      "glibc-locales"
      "nss-certs"
      
      "qtile"
      
      ))
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
                   "docker"
                   "scanner"
                   "libvirt"
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
         (target (uuid "tbd")))))

 (file-systems
  (cons* (file-system
          (mount-point "/")
          (device
           (uuid "tbd"
                 'ext4))
          (type "ext4"))
         %base-file-systems)))
