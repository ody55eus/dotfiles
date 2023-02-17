(define-module (home hosts ixy)
  #:use-module (rde features base)
  #:use-module (rde features system)
  #:use-module (rde features wm)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system mapped-devices)
  #:use-module (ice-9 match))


;;; Hardware/host specifis features

;; TODO: Switch from UUIDs to partition labels For better
;; reproducibilty and easier setup.  Grub doesn't support luks2 yet.

(define ixy-file-systems
  (list
   (file-system
     (mount-point "/")
     (device "/dev/sda3")
     (type "ext4"))
   ;; (file-system
   ;;   (mount-point "/boot/efi")
   ;;   (type "vfat")
   ;;   (device (file-system-label "EFI")))
   ))

(define-public %ixy-features
  (list
   (feature-host-info
    #:host-name "ixy"
    #:locale "en_US.utf8"
    ;; ls `guix build tzdata`/share/zoneinfo
    #:timezone  "Europe/Berlin")
   ;;; Allows to declare specific bootloader configuration,
   ;;; grub-efi-bootloader used by default
   (feature-bootloader
    #:bootloader-configuration (bootloader-configuration
                                (bootloader grub-bootloader)
                                (targets (list "/dev/sda"))
                                (menu-entries (list (menu-entry
                                                     (label "Guix Circe")
                                                     (device "/dev/sda4")
                                                     (linux "/gnu/store/s1c6qbvabrkijz6byg5ach40c0ncgkv6-linux-libre-6.1.8/bzImage")
                                                     (linux-arguments '("root=ccfdb723-4ce7-4edd-a2f1-8518af456e1f"
                                                                        "gnu.system=/gnu/store/mjhd3abb8g3l4ax1ipbh5kl47cnsadq5-system"
                                                                        "gnu.load=/gnu/store/mjhd3abb8g3l4ax1ipbh5kl47cnsadq5-system/boot"
                                                                        "modprobe.blacklist=usbmouse,usbkbd"
                                                                        "quiet"))
                                                     (initrd "/gnu/store/9xsp59aifw73j5pfvnn5njvr6c74w4v9-combined-initrd/initrd.img"))))
                                (keyboard-layout (keyboard-layout
                                                  "de" "neo"
                                                  #:options '("grp:shifts_toggle")))))
   (feature-file-systems
    ;; #:mapped-devices ixy-mapped-devices
    #:file-systems   ixy-file-systems)
   (feature kernel
            #:kernel linux
            #:initrd microcode-intel
            #:firmware (list linux-firmware))
   (feature-kanshi
    #:extra-config
    `((profile laptop ((output HDMI-A-1 enable)))
      (profile docked ((output HDMI-A-1 enable)
                       (output DP-2 scale 2)))))
   (feature-hidpi)))
