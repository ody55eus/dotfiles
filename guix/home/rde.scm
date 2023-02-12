(define-module (home rde)
  #:use-module (home base)
  #:use-module (home hosts ixy)
  #:use-module (home hosts live)

  #:use-module (rde features)
  #:use-module (rde features base)
  #:use-module (rde features gnupg)
  #:use-module (rde features security-token)
  #:use-module (rde features keyboard)
  #:use-module (rde features system)
  #:use-module (rde features xdg)
  #:use-module (rde features password-utils)
  #:use-module (rde features emacs-xyz)
  #:use-module (rde features mail)
  #:use-module (rde features irc)
  #:use-module (rde features networking)
  #:use-module (rde features clojure)
  #:use-module (contrib features javascript)

  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (rde home services i2p)
  #:use-module (rde home services emacs)
  #:use-module (rde home services wm)

  #:use-module (gnu home-services ssh)

  #:use-module (gnu packages)
  #:use-module (rde packages)

  #:use-module (guix gexp)
  #:use-module (guix inferior)
  #:use-module (guix channels)
  #:use-module (ice-9 match))

;;; Helpers

(define* (mail-acc id user #:optional (type 'gmail))
  "Make a simple mail-account with gmail type by default."
  (mail-account
   (id   id)
   (fqda user)
   (type type)))

(define* (mail-lst id fqda urls)
  "Make a simple mailing-list."
  (mailing-list
   (id   id)
   (fqda fqda)
   (config (l2md-repo
            (name (symbol->string id))
            (urls urls)))))

;;; Service extensions

(define emacs-extra-packages-service
  (simple-service
   'emacs-extra-packages
   home-emacs-service-type
   (home-emacs-extension
    (elisp-packages
     (append
      (strings->packages
       ;; "emacs-dirvish"
       "emacs-hl-todo"
       "emacs-yasnippet"
       "emacs-company"
       "emacs-consult-dir"
       "emacs-all-the-icons-completion" "emacs-all-the-icons-dired"
       "emacs-kind-icon"
       "emacs-nginx-mode" "emacs-yaml-mode"
       ;; "emacs-lispy"
       "emacs-ytdl"
       "emacs-multitran"
       ;; "emacs-minimap"
       "emacs-ement"
       "emacs-restart-emacs"
       "emacs-org-present"))))))

(define home-extra-packages-service
  (simple-service
   'home-profile-extra-packages
   home-profile-service-type
   (append
    (strings->packages
     ;; "alacritty-next"
     "figlet" ;; TODO: Move to emacs-artist-mode
     ;; "calibre"
     "icecat" ; "nyxt"
     "ungoogled-chromium-wayland" "ublock-origin-chromium"

     "utox" "qtox" "jami"

     "alsa-utils" "youtube-dl" "imv" "cozy"
     "pavucontrol" "wev"
     ;; "imagemagick"
     ;; "obs" "obs-wlrobs"
     "recutils" "binutils" "make"
     "fheroes2"

     "hicolor-icon-theme" "adwaita-icon-theme" "gnome-themes-extra"
     "papirus-icon-theme" "arc-theme"
     "thunar" "fd"
     ;; "glib:bin"

     ;; "libreoffice"
     "ffmpeg"
     "ripgrep" "curl"))))

;; (define sway-extra-config-service
;;   (simple-service
;;    'sway-extra-config
;;    home-sway-service-type
;;    `((output DP-2 scale 2)
;;      ;; (output eDP-1 disable)
;;      ,@(map (lambda (x) `(workspace ,x output DP-2)) (iota 8 1))

;;      ;; (workspace 9 output DP-2)
;;      ;; (workspace 10 output DP-2)

;;      ;; (bindswitch --reload --locked lid:on exec /run/setuid-programs/swaylock)

;;      (bindsym
;;       --locked $mod+Shift+t exec
;;       ,(file-append (@ (gnu packages music) playerctl) "/bin/playerctl")
;;       play-pause)

;;      (bindsym
;;       --locked $mod+Shift+n exec
;;       ,(file-append (@ (gnu packages music) playerctl) "/bin/playerctl")
;;       next)

;;      (bindsym $mod+Shift+o move workspace to output left)
;;      (bindsym $mod+Ctrl+o focus output left)
;;      (input type:touchpad
;;             ;; TODO: Move it to feature-sway or feature-mouse?
;;             (;; (natural_scroll enabled)
;;              (tap enabled)))

;;      ;; (xwayland disable)
;;      (bindsym $mod+Return exec alacritty)
;;      (bindsym $mod+Shift+Return exec emacs))))

;; (define i2pd-add-ilita-irc-service
;;   (simple-service
;;    'i2pd-add-ilita-irc
;;    home-i2pd-service-type
;;    (home-i2pd-extension
;;     (tunnels-conf
;;      `((IRC-ILITA ((type . client)
;;                    (address . 127.0.0.1)
;;                    (port . 6669)
;;                    (destination . irc.ilita.i2p)
;;                    (destinationport . 6667)
;;                    (keys . ilita-keys.dat))))))))

(define ssh-extra-config-service
  (simple-service
   'ssh-extra-config
   home-ssh-service-type
   (home-ssh-extension
    (extra-config
     ;; (append
      ;; TODO: Move it feature-qemu?
      ;; (map (lambda (id)
      ;;        (ssh-host
      ;;         (host (format #f "qemu~a" id))
      ;;         (options
      ;;          `((host-name . "localhost")
      ;;            (port . ,(+ 10020 id))))))
      ;;      (iota 4))
      (list
       (ssh-host
        (host "ithaca")
        (options
         '((host-name . "192.168.20.87")
           (port . 22)
           (control-master . "auto")
           (control-path . "~/.ssh/master-%r@%h:%p")
           (compression . #t))))
       (ssh-host
        (host "nasserver")
        (options
         '((host-name . "192.168.20.20")
           (port . 2123)
           (control-master . "auto")
           (control-path . "~/.ssh/master-%r@%h:%p")
           (compression . #t))))))
    (toplevel-options
     '((host-key-algorithms . "+ssh-rsa")
       (pubkey-accepted-key-types . "+ssh-rsa"))))))

;;; User-specific features with personal preferences

;; Initial user's password hash will be available in store, so use this
;; feature with care (display (crypt "hi" "$6$abc"))

(define %ody-features
  (list
   (feature-user-info
    #:user-name "jp"
    #:full-name "Jonathan Pieper"
    #:email "ody55eus@mailbox.org"
    #:user-initial-password-hash
    "$6$c8P$kB/4pLdnv4.9DR8n6LdLf5a9qaSyyqmX3GW1hP4y42gnaIgfcqetYe91YZerZbYVG/Nr.FLM6ZNhJ7iOyyNfo0"
    ;; (crypt "YOURPASSWD" "$6$SALT")

    ;; WARNING: This option can reduce the explorability by hiding
    ;; some helpful messages and parts of the interface for the sake
    ;; of minimalistic, less distractive and clean look.  Generally
    ;; it's not recommended to use it.
    #:emacs-advanced-user? #t)
   (feature-gnupg
    #:gpg-primary-key "885B941B822133216D960E4CDE2AD6CF2474B880")
   (feature-security-token)
   (feature-mail-settings
    #:mail-accounts (list (mail-acc 'work       "ody55eus@mailbox.org" )
                          (mail-acc 'personal   "jonathan@ich-habe-fertig.com" ))
    #:mailing-lists (list (mail-lst 'guix-devel "guix-devel@gnu.org"
                                    '("https://yhetil.org/guix-devel/0"))
                          (mail-lst 'guix-bugs "guix-bugs@gnu.org"
                                    '("https://yhetil.org/guix-bugs/0"))
                          (mail-lst 'guix-patches "guix-patches@gnu.org"
                                    '("https://yhetil.org/guix-patches/1"))))

   ;; (feature-irc-settings
   ;;  #:irc-accounts (list
   ;;                  (irc-account
   ;;                   (id 'srht)
   ;;                   (network "chat.sr.ht")
   ;;                   (bouncer? #t)
   ;;                   (nick "abcdw"))
   ;;                  (irc-account
   ;;                   (id 'libera)
   ;;                   (network "irc.libera.chat")
   ;;                   (nick "abcdw"))
   ;;                  (irc-account
   ;;                   (id 'oftc)
   ;;                   (network "irc.oftc.net")
   ;;                   (nick "abcdw"))))

   (feature-custom-services
    #:feature-name-prefix 'ody
    #:home-services
    (list
     emacs-extra-packages-service
     home-extra-packages-service
     ;; sway-extra-config-service
     ssh-extra-config-service
     ;;i2pd-add-ilita-irc-service
     ))

   ;; (feature-ssh-proxy #:host "ithaca" #:auto-start? #f)
   ;; (feature-ssh-proxy #:host "nasserver" #:name "hundredrps"
   ;;                    #:proxy-string "50080:localhost:8080"
   ;;                    #:reverse? #t
   ;;                    #:auto-start? #f)

   (feature-xdg
    #:xdg-user-directories-configuration
    (home-xdg-user-directories-configuration
     (music "$HOME/music")
     (videos "$HOME/vids")
     (pictures "$HOME/pics")
     (documents "$HOME/docs")
     (download "$HOME/dl")
     (desktop "$HOME")
     (publicshare "$HOME")
     (templates "$HOME")))

   ;; (feature-yggdrasil)
   ;; (feature-i2pd
   ;;  #:outproxy 'http://acetone.i2p:8888
   ;;  ;; 'purokishi.i2p
   ;;  #:less-anonymous? #t)

   (feature-emacs-keycast #:turn-on? #t)

   (feature-emacs-tempel
    #:default-templates? #t
    #:templates
    `(fundamental-mode
      ,#~""
      (t (format-time-string "%Y-%m-%d"))
      ;; TODO: Move to feature-guix
      ;; ,((@ (rde gexp) slurp-file-like)
      ;;   (file-append ((@ (guix packages) package-source)
      ;;                 (@ (gnu packages package-management) guix))
      ;;                "/etc/snippets/tempel/text-mode"))
      ))
   ;; (feature-emacs-spelling
   ;;  #:spelling-program (@ (gnu packages hunspell) hunspell)
   ;;  #:spelling-dictionaries (strings->packages
   ;;                           "hunspell-dict-en"
   ;;                           "hunspell-dict-de"))
   (feature-emacs-git
    #:project-directory "~/share/Projects/Code")
   (feature-emacs-org
    #:org-directory "~/ZK"
    #:org-indent? #f
    #:org-capture-templates
    `(("t" "Todo" entry (file+headline "" "Tasks") ;; org-default-notes-file
       "* TODO %?\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t)))
   (feature-emacs-org-roam
    ;; TODO: Rewrite to states
    #:org-roam-directory "~/ZK")
   (feature-emacs-org-agenda
    #:org-agenda-files '("~/ZK/Habits.org"))
   (feature-emacs-elfeed
    #:elfeed-org-files '("~/ZK/rss.org"))

   (feature-javascript)

   ;; TODO: move feature to general, move extra configuration to service.
;;    (feature-notmuch
;;     ;; TODO: Add integration with mail-lists
;;     ;; `notmuch-show-stash-mlarchive-link-alist'
;;     #:extra-tag-updates-post
;;     '("notmuch tag +guix-home -- 'thread:\"\
;; {((subject:guix and subject:home) or (subject:service and subject:home) or \
;; subject:/home:/) and tag:new}\"'")
;;     #:notmuch-saved-searches
;;     (cons*
;;      ;; TODO: Add tag:unread to all inboxes.  Revisit archive workflow.
;;      '(:name "Work Inbox" :query "tag:work and tag:inbox and tag:unread" :key "W")
;;      '(:name "Personal Inbox" :query "tag:personal and tag:inbox" :key "P")
;;      '(:name "Guix Home Inbox" :key "H" :query "tag:guix-home and tag:unread")
;;      '(:name "RDE Inbox"       :key "R"
;;              :query "(to:/rde/ or cc:/rde/) and tag:unread")

;;      ;; '(:name "Watching" :query "thread:{tag:watch} and tag:unread" :key "tw")
;;      %rde-notmuch-saved-searches))

   (feature-keyboard
    ;; To get all available options, layouts and variants run:
    ;; cat `guix build xkeyboard-config`/share/X11/xkb/rules/evdev.lst
    #:keyboard-layout
    (keyboard-layout
     "de" "neo"
     #:options '("grp:shifts_toggle")))))

(define %guest-features
  (list
   (feature-user-info
    #:user-name "guest"
    #:full-name "guest user"
    #:email "guest@rde"
    ;; (crypt "guest" "$6$abc")
    #:user-initial-password-hash
    "$6$abc$9a9KlQ2jHee45D./UOzUZWLHjI/atvz2Dp6.Zz6hjRcP2KJv\
G9.lc/f.U9QxNW1.2MZdV1KzW6uMJ0t23KKoN/")

   (feature-keyboard
    ;; To get all available options, layouts and variants run:
    ;; cat `guix build xkeyboard-config`/share/X11/xkb/rules/evdev.lst
    #:keyboard-layout
    (keyboard-layout
     "de"
     "neo"
     #:options '("grp:shifts_toggle")))))

;;; ixy

(define-public ixy-config
  (rde-config
   (features
    (append
     %ody-features
     %all-features
     %ixy-features))))

(define-public ixy-os
  (rde-config-operating-system ixy-config))

(define-public ixy-he
  (rde-config-home-environment ixy-config))

;;; live

(use-modules (srfi srfi-1)
             (rde features version-control))

(define sway-wlr-settings-service
  (simple-service
   'sway-wlr-settings
   home-environment-variables-service-type
   ;; Make sway work on virtual gpu in qemu
   `(("WLR_RENDERER_ALLOW_SOFTWARE" . "1")
     ("WLR_NO_HARDWARE_CURSORS" . "1"))))

(define sway-live-extra-config-service
  (simple-service
   'sway-output-settings
   home-sway-service-type
   `((output Virtual-1 mode 1920x1080 scale 2)
     (exec emacs --eval "'(info \"(rde)Getting Started\")'"))))

(define home-profile-live-extra-packages-service
  (simple-service
   'home-profile-live-extra-packages
   home-profile-service-type
   (append
    (strings->packages
     "icecat"
     "ungoogled-chromium-wayland" "ublock-origin-chromium"
     "imv" "wev"
     "make"
     "adwaita-icon-theme" "gnome-themes-extra"
     "hicolor-icon-theme" ;; needed for nm icons

     "ripgrep" "curl"))))

(define example-configs-service
  (simple-service
   'live-example-configs
   home-shepherd-service-type
   (list
    (shepherd-service
     (documentation "Create ~/rde-configs.")
     (requirement '())
     (provision '(rde-configs))
     (start
      (with-imported-modules '((guix build utils))
        #~(lambda ()
            (let ((rde-configs #$(local-file
                                  ".." "ody-configs"
                                  #:recursive? #t
                                  #:select?
                                  (lambda (file _)
                                    (not (string=? (basename file) "build")))))
                  (output (string-append (getenv "HOME") "/ody-configs")))
              (when (not (file-exists? output))
                (mkdir-p output)
                (copy-recursively
                 rde-configs
                 output
                 #:copy-file (lambda (f t)
                               (copy-file f t)
                               (make-file-writable t)))
                ;; MAYBE: take this value from rde-config
                (system* #$(file-append (@ (gnu packages shellutils) direnv)
                                        "/bin/direnv") "allow" output))))))
     (one-shot? #t)))))

(define live-custom-services
  (feature-custom-services
    #:feature-name-prefix 'live
    #:home-services
    (list
     ;; example-configs-service
     sway-live-extra-config-service
     sway-wlr-settings-service
     home-profile-live-extra-packages-service)))

;; TODO: Pull channels from lock file in advance and link them to example-config
;; TODO: Add auto-login

(define-public live-config
  (rde-config
   (integrate-he-in-os? #t)
   (features
    (append
     %guest-features
     (list live-custom-services)

     (remove
      (lambda (f) (member (feature-name f) '(git markdown)))
      %general-features)
     (list
      (feature-git #:sign-commits? #f)
      (feature-hidpi))

     %live-features))))

(define-public live-os
  (rde-config-operating-system live-config))

;;; Dispatcher, which helps to return various values based on environment
;;; variable value.

(define (dispatcher)
  (let ((rde-target (getenv "TARGET")))
    (match rde-target
      ("ixy-home" ixy-he)
      ("ixy-system" ixy-os)
      ("live-system" live-os)
      (_ ixy-he))))

(dispatcher)