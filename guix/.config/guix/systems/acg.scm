;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules (gnu)
             (nongnu packages linux))
(use-service-modules
  databases
  desktop
  networking
  ssh
  xorg)

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
                %base-user-accounts))
  (packages
    (append
      (list
       (specification->package "awesome")
       (specification->package "nss-certs"))
      %base-packages))
  (services
   (append
    (list
     (service mysql-service-type)
     (set-xorg-configuration
      (xorg-configuration
       (keyboard-layout keyboard-layout))))
    %desktop-services))
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
