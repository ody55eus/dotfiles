(define-module (home hosts ixy)
  #:use-module (rde features base)
  #:use-module (rde features system)
  #:use-module (rde features wm)
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
     (device
      (uuid "0cce4a5f-deda-4877-b628-4188d944dbbb"
            'ext4))
     (type "ext4"))
   ))

(define-public %ixy-features
  (list
   (feature-host-info
    #:host-name "ixy"
    ;; ls `guix build tzdata`/share/zoneinfo
    #:timezone  "Europe/Berlin")
   ;;; Allows to declare specific bootloader configuration,
   ;;; grub-efi-bootloader used by default
   (feature-bootloader)
   (feature-file-systems
    ;; #:mapped-devices ixy-mapped-devices
    #:file-systems   ixy-file-systems)
   (feature-kanshi
    #:extra-config
    `((profile laptop ((output eDP-1 enable)))
      (profile docked ((output eDP-1 enable)
                       (output DP-2 scale 2)))))
   (feature-hidpi)))
