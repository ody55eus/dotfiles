;; This is the configuration for nonguix build server.

(use-modules (gnu))
(use-service-modules admin
                     avahi
                     certbot
                     cuirass
                     mcron
                     networking
                     ssh
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
               (mount-point "/data")
               (device "f22fb592-ac5b-4ffe-af63-16deeed3caff")
               (type "ext4")))
            %base-file-systems))

  (packages
   (append
    (map (compose list specification->package+output)
     (list
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
       "zstd"))
    %base-packages))

  (services
    (cons*
      (service avahi-service-type)

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
            (service iptables-service-type
              (iptables-configuration
                (ipv4-rules (plain-file "iptables.rules" "*filter
-A INPUT -p tcp --dport 5522 ! -s 127.0.0.1 -j REJECT
-A INPUT -p tcp --dport 5555:5558 ! -s 127.0.0.1 -j REJECT
-A INPUT -p tcp --dport 8080:8081 ! -s 127.0.0.1 -j REJECT
COMMIT
"))))

            (service nginx-service-type
              (nginx-configuration
               (upstream-blocks
                (list
                 (nginx-upstream-configuration
                   (name "guix-cuirass")
                   (servers (list "localhost:8081")))
                 (nginx-upstream-configuration
                   (name "guix-publish")
                   (servers (list "localhost:8080")))))
               (server-blocks
                (list
                 (nginx-server-configuration
                   (server-name '("guix.ody55eus.de"))
                   (listen '("443 ssl" "[::]:443 ssl"))
                   (locations
                    (list
                     (nginx-location-configuration
                       (uri "~ ^/admin")
                       (body
                        (list "if ($ssl_client_verify != SUCCESS) { return 403; } proxy_pass http://guix-cuirass;")))
                     (nginx-location-configuration
                       (uri "/")
                       (body '("proxy_pass http://guix-cuirass;")))))
                   (ssl-certificate "/etc/letsencrypt/live/ody55eus.de/fullchain.pem")
                   (ssl-certificate-key "/etc/letsencrypt/live/ody55eus.de/privkey.pem"))
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
                   (ssl-certificate "/etc/letsencrypt/live/ody55eus.de/fullchain.pem")
                   (ssl-certificate-key "/etc/letsencrypt/live/ody55eus.de/privkey.pem"))))))

            (service openssh-service-type
              (openssh-configuration
                (authorized-keys
                 `(("jp" ,(local-file "jp.pub"))))
                (password-authentication? #f)
                (port-number 2123)))

            (service ntp-service-type)
            (service dhcp-service-type)
      ;; (service static-networking-service-type
      ;;   (list (static-networking
      ;;     (addresses
      ;;       (list (network-address
      ;;               (device "enp9s0")
      ;;               (value "144.76.7.123/27"))
      ;;             (network-address
      ;;               (device "enp9s0")
      ;;               (ipv6? #t)
      ;;               (value "2a01:4f8:190:8242::1/64"))))
      ;;     (routes
      ;;       (list (network-route
      ;;               (destination "default")
      ;;               (device "enp9s0")
      ;;               (gateway "144.76.7.97"))
      ;;             (network-route
      ;;               (destination "default")
      ;;               (device "enp9s0")
      ;;               (ipv6? #t)
      ;;               (gateway "fe80::1"))))
      ;;     (name-servers '("185.12.64.1" "185.12.64.2"
      ;;                     "2001:4860:4860::8888"
      ;;                     "2001:4860:4860::8844")))))

            (simple-service 'cron-jobs
                            mcron-service-type
                            (list garbage-collector-job))

            ;; Browse through bash history via PageUp/PageDown
            (simple-service 'inputrc etc-service-type
             `(("inputrc" ,(plain-file "inputrc"
                             (string-append
                               "\"\\e[5~\": history-search-backward\n"
                               "\"\\e[6~\": history-search-forward\n")))))

            (modify-services %base-services
             (guix-service-type
              config => (guix-configuration
                         (inherit config)
                         (authorized-keys ;;(append
                                           %default-authorized-guix-keys
                                           ;; (list (local-file "keys/guix/worker-vm.pub")))
                                           )))))))
