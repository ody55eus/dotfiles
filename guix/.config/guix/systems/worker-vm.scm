;; Cuirass worker VM. Reconfigure inside the VM with:
;;   guix system reconfigure /etc/worker-vm.scm

(use-modules (gnu) (guix) (srfi srfi-1))
(use-service-modules avahi
                     cuirass
                     mcron
                     networking
                     ssh)
(use-package-modules bootloaders certs fonts nvi
                     package-management wget xorg)

;; Run the garbe collector every day at 3:00 AM
(define garbage-collector-job
  #~(job "0 3 * * *"
         "guix gc -F 50G"))

;; Restart the cuiras remote worker every night
(define restart-remote-worker-job
  #~(job "0 2 * * *"
         "herd restart cuirass-remote-worker"))

(operating-system
  (host-name "worker1")
  (timezone "Etc/UTC")
  (locale "en_US.utf8")
  (keyboard-layout (keyboard-layout "de" "neo"))

  ;; Label for the GRUB boot menu.
  (label (string-append "GNU Guix " (package-version guix)))

  (firmware '())

  ;; Below we assume /dev/vda is the VM's hard disk.
  ;; Adjust as needed.
  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (targets '("/dev/vda"))
               (terminal-outputs '(console))))
  (file-systems (cons (file-system
                        (mount-point "/")
                        (device "/dev/vda2")
                        (type "ext4"))
                      %base-file-systems))

  (users (cons (user-account
                (name "guest")
                (comment "GNU Guix Live")
                (password "")                     ;no password
                (group "users")
                (supplementary-groups '("wheel" "netdev"
                                        "audio" "video")))
               %base-user-accounts))

  ;; Our /etc/sudoers file.  Since 'guest' initially has an empty password,
  ;; allow for password-less sudo.
  (sudoers-file (plain-file "sudoers" "\
root ALL=(ALL) ALL
%wheel ALL=NOPASSWD: ALL\n"))

  (packages (append (list font-bitstream-vera nss-certs nvi wget)
                    %base-packages))

  (services
    (cons*
      (service avahi-service-type)

      (service cuirass-remote-worker-service-type
        (cuirass-remote-worker-configuration
          (private-key "/etc/guix/signing-key.sec")
          (public-key "/etc/guix/signing-key.pub")
          (server "10.0.2.2:5555")
          (substitute-urls '("https://ci.guix.gnu.org"
                             "https://substitutes.nonguix.org"))
          (systems '("x86_64-linux"))
          (workers 2)))

      (service dhcp-client-service-type)

      (service ntp-service-type)

      (service openssh-service-type
        (openssh-configuration
          (allow-empty-passwords? #t)
          (permit-root-login #t)))

      (simple-service 'cron-jobs
                      mcron-service-type
                      (list garbage-collector-job
                            restart-remote-worker-job))

      (modify-services %base-services
        (guix-service-type config =>
          (guix-configuration
            (inherit config)
            (authorized-keys (append
                               %default-authorized-guix-keys
                               (list (plain-file "substitutes.nonguix.org.pub"
                                       "(public-key
(ecc
 (curve Ed25519)
 (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)
 )
)"))))
            (extra-options
             ;; TODO: set "correct" values
             '("--max-jobs=2" "--cores=4"))))))))
