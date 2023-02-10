;; This is the configuration for nonguix build server.

(use-modules (gnu))
(use-package-modules cups printers)
(use-service-modules admin
                     avahi
                     certbot
                     cuirass
                     cups
                     desktop
                     mcron
                     networking
                     ssh
                     xorg
                     virtualization
                     nix
                     docker
                     web)

(define %cuirass-specs
  #~(list
     (specification
      (name "dinos")
      (build '(channels dinos))
      (channels
        (list %default-guix-channel
              (channel
                (name 'dinos)
                (url "https://gitlab.ody5.de/ody55eus/dinos.git")
                (introduction
                  (make-channel-introduction
                    "e802e65a76e49435ded5ef909120b2b84f2e262b"
                    (openpgp-fingerprint
                      "885B 941B 8221 3321 6D96 0E4C DE2A D6CF 2474 B880"))))))
      (priority 0)
      (systems '("x86_64-linux")))))

(define %nginx-deploy-hook
  (program-file
   "nginx-deploy-hook"
   #~(let ((pid (call-with-input-file "/var/run/nginx/pid" read)))
       (kill pid SIGHUP))))

;; Run the garbe collector every day at 4:00 AM
(define garbage-collector-job
  #~(job "0 4 * * *"
         "guix gc"))

(operating-system
  (locale "de_DE.utf8")
  (timezone "Europe/Berlin")
  (keyboard-layout (keyboard-layout "de" "neo"))
  (host-name "neptun")
  (users (cons* (user-account
                  (name "jp")
                  (comment "Jonathan Pieper")
                  (group "users")
                  (home-directory "/home/jp")
                  (supplementary-groups
                    '("wheel" "netdev" "kvm")))
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
  (bootloader
    (bootloader-configuration
      (bootloader grub-bootloader)
      (targets '("/dev/sda"))
      (keyboard-layout keyboard-layout)))
  (swap-devices
    (list (swap-space (target (uuid "5e3f3adf-c169-4e92-8265-2366f5b0aa3f")))))
  (file-systems
    (append (list
             (file-system
               (mount-point "/")
               (device
                 (uuid "24936c30-c01d-4fe9-9160-9b2b11e9db0f"
                       'ext4))
               (type "ext4"))
             (file-system
               (mount-point "/mnt")
               (device "/dev/sdb1")
               (type "ext4")))
            %base-file-systems))

  (packages
   (append
    (map (compose list specification->package+output)
     (list
       "awesome"
       "curl"
       "git"
       "htop"
       "ncdu"
       "neovim"
       "nmap"
       "nss-certs"
       "qemu"
       "screen"
       "smartmontools"
       "wget"
       "zsh"
       "zstd"))
    %base-packages))

  (services
    (cons*
      (service avahi-service-type)
      (service cups-service-type
               (cups-configuration
                 (extensions
                   (list cups-filters
                         epson-inkjet-printer-escpr ;; Epson
                         ; brlaser                  ;; Brother
                         ; hplip-minimal            ;; HP
                         ; splix                    ;; Samsung, Xerox, Lexmark, Toshiba, Dell
                         ; (specification->package+output "splix:ppd")
                         ))
                 (listen '("*:631"))
                 (browsing? #t)
                 (browse-web-if? #t)
                 (web-interface? #t)))

            (service certbot-service-type
              (certbot-configuration
               (certificates
                (list
                 (certificate-configuration
                   (deploy-hook %nginx-deploy-hook)
                   (domains '("guix.ody55eus.de"
                              "p.ody55eus.de"
                              "gitlab.ody55eus.de")))))
                   (email "jpieper+https@mailbox.org")
                   (webroot "/srv/http")))

            (service cuirass-service-type
              (cuirass-configuration
                (remote-server
                  (cuirass-remote-server-configuration
                    (private-key "/etc/guix/signing-key.sec")
                    (public-key "/etc/guix/signing-key.pub")
                    (publish? #t)
                    (trigger-url "http://localhost:8080")))
                (specifications %cuirass-specs)
                (use-substitutes? #t)))

            (service guix-publish-service-type
              (guix-publish-configuration
                ;; Requires manual: sudo mkdir /var/cache/publish
                ;; sudo chown -R guix-publish:guix-publish /var/cache/publish
                (cache "/var/cache/publish")
                (compression '(("zstd" 19)))
                (port 8080)))

            ;; TODO: base on http://issues.guix.gnu.org/48975
;;             (service iptables-service-type
;;               (iptables-configuration
;;                 (ipv4-rules (plain-file "iptables.rules" "*filter
;; -A INPUT -p tcp --dport 5522 ! -s 127.0.0.1 -j REJECT
;; -A INPUT -p tcp --dport 5555:5558 ! -s 127.0.0.1 -j REJECT
;; -A INPUT -p tcp --dport 8080:8081 ! -s 127.0.0.1 -j REJECT
;; COMMIT
;; "))))

            (service nginx-service-type
              (nginx-configuration
               (upstream-blocks
                 (list
                   (nginx-upstream-configuration
                     (name "guix-cuirass")
                     (servers (list "localhost:8081")))
                   (nginx-upstream-configuration
                     (name "guix-publish")
                     (servers (list "localhost:8080")))
                   (nginx-upstream-configuration
                     (name "guix-cups")
                     (servers (list "localhost:631")))))
               (server-blocks
                 (list
                   (nginx-server-configuration
                     (server-name '("cups.ody55eus.de"))
                     (listen '("443 ssl" "[::]:443 ssl"))
                     (locations
                       (list
                         (nginx-location-configuration
                           (uri "/")
                           (body '("proxy_pass http://guix-cups;")))))
                     (ssl-certificate "/etc/letsencrypt/live/p.ody55eus.de/fullchain.pem")
                     (ssl-certificate-key "/etc/letsencrypt/live/p.ody55eus.de/privkey.pem"))
                   (nginx-server-configuration
                     (server-name '("guix.ody55eus.de"))
                     (listen '("443 ssl" "[::]:443 ssl"))
                     (locations
                       (list
                         (nginx-location-configuration
                           (uri "/")
                           (body '("proxy_pass http://guix-cuirass;")))))
                     (ssl-certificate "/etc/letsencrypt/live/p.ody55eus.de/fullchain.pem")
                     (ssl-certificate-key "/etc/letsencrypt/live/p.ody55eus.de/privkey.pem"))
                   (nginx-server-configuration
                     (server-name '("p.ody55eus.de"))
                     (listen '("443 ssl" "[::]:443 ssl"))
                     (raw-content '("rewrite ^//(.*)$ /$1 redirect;"))
                     (locations
                       (list
                         (nginx-location-configuration
                           (uri "/signing-key.pub")
                           (body '("proxy_pass http://guix-publish;")))
                         (nginx-location-configuration
                           (uri "/file/")
                           (body '("proxy_pass http://guix-publish;")))
                         (nginx-location-configuration
                           (uri "/log/")
                           (body '("proxy_pass http://guix-publish;")))
                         (nginx-location-configuration
                           (uri "/nix-cache-info")
                           (body (list
                                   "proxy_pass http://guix-publish;"
                                   "proxy_hide_header Set-Cookie;")))
                         (nginx-location-configuration
                           (uri "/nar/")
                           (body (list
                                   "proxy_pass http://guix-publish;"
                                   "client_body_buffer_size 256k;"
                                   ;; Nars are already compressed. -> no perf change
                                   "gzip off;"
                                   "proxy_pass_header Cache-Control;")))
                         (nginx-location-configuration
                           (uri "~ \\.narinfo$")
                           (body
                             (list
                               "proxy_pass http://guix-publish;"
                               "client_body_buffer_size 128k;"
                               "proxy_connect_timeout 2s;"
                               "proxy_read_timeout 2s;"
                               "proxy_send_timeout 2s;"
                               "proxy_pass_header Cache-Control;"
                               "proxy_ignore_client_abort on;")))))
                     (ssl-certificate "/etc/letsencrypt/live/p.ody55eus.de/fullchain.pem")
                     (ssl-certificate-key "/etc/letsencrypt/live/p.ody55eus.de/privkey.pem"))))))

            (service openssh-service-type
              (openssh-configuration
                (authorized-keys
                 `(("jp" ,(local-file "jp.pub"))  
                   ("bkp" ,(local-file "bkp.pub"))))
                (password-authentication? #t)
                (permit-root-login #f)
                (port-number 2123)))

            (service ntp-service-type)
       (service static-networking-service-type
         (list (static-networking
           (addresses
             (list (network-address
                     (device "enp2s0")
                     (value "192.168.20.20/24"))
                   (network-address
                     (device "enp2s0")
                     (ipv6? #t)
                     (value "2a02:2455:cea:eb00::1/64"))))
           (routes
             (list (network-route
                     (destination "default")
                     (device "enp2s0")
                     (gateway "192.168.20.1"))
                   (network-route
                     (destination "default")
                     (device "enp2s0")
                     (ipv6? #t)
                     (gateway "fe80::1"))))
           (name-servers '("192.168.20.1" "1.1.1.1"
                           "2001:4860:4860::8888"
                           "2001:4860:4860::8844")))))

            (simple-service 'cron-jobs
                            mcron-service-type
                            (list garbage-collector-job))

            ;; Browse through bash history via PageUp/PageDown
            (simple-service 'inputrc etc-service-type
             `(("inputrc" ,(plain-file "inputrc"
                             (string-append
                               "\"\\e[5~\": history-search-backward\n"
                               "\"\\e[6~\": history-search-forward\n")))))

                (service nix-service-type)
                (service docker-service-type)
                (service slim-service-type (slim-configuration
                                             (display ":0")
                                             (vt "vt7")
                                             (allow-empty-passwords? #f)
                                             (auto-login? #t)
                                             (default-user "jp")
                                             (xorg-configuration
                                               (xorg-configuration
                                                 (keyboard-layout keyboard-layout)))
                                             ))

    (modify-services %desktop-services
                     (delete avahi-service-type)
                     (delete network-manager-service-type)
                     (delete ntp-service-type)
                     (delete gdm-service-type)
             (guix-service-type
              config => (guix-configuration
                         (inherit config)
                         (substitute-urls (append 
                                            %default-substitute-urls
                                            (list "https://substitutes.nonguix.org")))
                         (authorized-keys (append
                                            (list 
                                              (local-file "keys/guix/nonguix.pub")
                                              (local-file "keys/guix/neptun.pub"))
                                            %default-authorized-guix-keys))))))))

