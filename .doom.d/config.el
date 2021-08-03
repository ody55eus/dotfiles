;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
(menu-bar-mode 1)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Jonathan Pieper"
      user-mail-address "ody55eus@mailbox.org")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-font (font-spec :family "Mononoki Nerd Font" :size 15)
;;      doom-variable-pitch-font (font-spec :family "Mononoki Nerd Font" :size 15))

(setq doom-theme 'doom-gruvbox)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(after! org
  (setq org-directory "~/org/"
        org-agenda-files '("~/org/agenda.org")
        org-ellipsis " ▼ "
        org-log-done 'time
        org-journal-dir "~/Org/journal/"
        org-journal-date-format "%B %d, %Y (%A) "
        org-journal-file-format "%Y-%m-%d.org"
        org-default-notes-file (concat org-directory "/notes.org")
        org-log-done-with-time t
        org-log-done 'note
        org-todo-keywords '((type "EPIC(e)" "PROJ(p)" "TODO(t)" "STRT(s)"
                                      "WAIT(w)" "HOLD(h)" "IDEA(i)" "|"
                                      "DONE(d)" "KILL(k)" "CANCELLED(c)"))
        org-todo-keyword-faces '(
                                 ("EPIC" . (:foreground "purple" :weight "bold"))
                                 ("PROJ" . "green")
                                 ("TODO" . org-warning)
                                 ("STRT" . "yellow")
                                 ("WAIT" . (:foreground "yellow" :weight "italic"))
                                 ("HOLD" . (:foreground "yellow" :underline))
                                 ("IDEA" . (:background "white" :foreground "blue"))
                                 ("KILL" . "red")
                                 ("CANCELLED" . (:foreground "red" :weight "bold"))
                                )
        org-capture-templates '(
                                ("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
                                 "* TODO %?\n %i\n %a")
                                ("e" "Epic" entry (file+headline "~/org/epic.org" "Epic")
                                 "* EPIC %?\n %i\n %a")
                                ("j" "Journal Entry" entry (file+datetree "~/org/journal.org")
                                 "* %?\nEntered on %U\n %i\n %a")
                               )
        )
  (require 'org-bullets)
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))) ; Enable org-mode bullets
  (setq org-roam-directory (file-truename "~/org")      ; Set org-roam directory
        org-roam-v2-ack t)                              ; Disable Warning for org-roam v2
  (org-roam-setup)                                      ; Setup Org-Roam
  )

(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 1.2))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.0))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
  )


(map! :leader
      (:prefix-map ("n" . "notes")
       (:prefix ("r" . "roam")
       :desc "Toggle org-roam Buffer" :n "l" #'org-roam-buffer-toggle
       :desc "Capture new org-roam Node" :n "n" #'org-roam-capture
       :desc "Capture new org-mode Node" :n "n" #'org-roam-capture
       :desc "Find org-roam Node" :n "f" #'org-roam-node-find
       :desc "Insert org-roam Node" :n "i" #'org-roam-node-insert
       :desc "Complete org-roam " :n "c" #'org-roam-complete-at-point)
       )
      )
;;(map! :leader :desc "Capture new org-roam Node" :n "n r n" #'org-roam-capture)
;;(map! :leader :desc "Find org-roam Node" :n "n r f" #'org-roam-node-find)
;;(map! :leader :desc "Insert org-roam Node" :n "n r i" #'org-roam-node-insert)
;;(map! :leader :desc "Complete org-roam " :n "n r c" #'org-roam-node-insert)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
(use-package dashboard
  :init      ;; tweak dashboard config before loading it
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Emacs Is More Than A Text Editor!")
  ;;(setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  (setq dashboard-startup-banner "~/.config/doom/doom-emacs-dash.png")  ;; use custom image as banner
  (setq dashboard-center-content nil) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 5)
                          (agenda . 5 )
                          (bookmarks . 5)
                          (projects . 5)
                          (registers . 5)))
  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text")
			      (bookmarks . "book"))))

(setq doom-fallback-buffer "*dashboard*")
(map! :leader
      (:prefix ("d" . "dired")
       :desc "Open dired" "d" #'dired
       :desc "Dired jump to current" "j" #'dired-jump)
      (:after dired
       (:map dired-mode-map
        :desc "Peep-dired image previews" "d p" #'peep-dired
        :desc "Dired view file" "d v" #'dired-view-file)))
;; Make 'h' and 'l' go back and forward in dired. Much faster to navigate the directory structure!
(evil-define-key 'normal dired-mode-map
  (kbd "M-RET") 'dired-display-file
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-open-file ; use dired-find-file instead of dired-open.
  (kbd "m") 'dired-mark
  (kbd "t") 'dired-toggle-marks
  (kbd "u") 'dired-unmark
  (kbd "C") 'dired-do-copy
  (kbd "D") 'dired-do-delete
  (kbd "J") 'dired-goto-file
  (kbd "M") 'dired-chmod
  (kbd "O") 'dired-chown
  (kbd "P") 'dired-do-print
  (kbd "R") 'dired-rename
  (kbd "T") 'dired-do-touch
  (kbd "Y") 'dired-copy-filenamecopy-filename-as-kill ; copies filename to kill ring.
  (kbd "+") 'dired-create-directory
  (kbd "-") 'dired-up-directory
  (kbd "% l") 'dired-downcase
  (kbd "% u") 'dired-upcase
  (kbd "; d") 'epa-dired-do-decrypt
  (kbd "; e") 'epa-dired-do-encrypt)
;; If peep-dired is enabled, you will get image previews as you go up/down with 'j' and 'k'
(evil-define-key 'normal peep-dired-mode-map
  (kbd "j") 'peep-dired-next-file
  (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)
;; Get file icons in dired
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
;; With dired-open plugin, you can launch external programs for certain extensions
;; For example, I set all .png files to open in 'sxiv' and all .mp4 files to open in 'mpv'
(setq dired-open-extensions '(("gif" . "sxiv")
                              ("jpg" . "sxiv")
                              ("png" . "sxiv")
                              ("mkv" . "mpv")
                              ("mp4" . "mpv")))

(pdf-tools-install)  ;; Activating pdf-tools
