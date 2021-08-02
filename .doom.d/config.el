;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


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
  (setq org-directory "~/org/")
  (setq org-agenda-files '("~/org/agenda.org"))
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-log-done-with-time t)
  (setq org-log-done 'note)
  (setq org-todo-keywords '((type "EPIC(e)" "PROJ(p)" "TODO(t)" "STRT(s)"
                                      "WAIT(w)" "HOLD(h)" "IDEA(i)" "|"
                                      "DONE(d)" "KILL(k)" "CANCELLED(c)")))
  (setq org-todo-keyword-faces '(
                                 ("EPIC" . (:foreground "purple" :weight "bold"))
                                 ("PROJ" . "green")
                                 ("TODO" . org-warning)
                                 ("STRT" . "yellow")
                                 ("WAIT" . (:foreground "yellow" :weight "italic"))
                                 ("HOLD" . (:foreground "yellow" :underline))
                                 ("IDEA" . (:background "white" :foreground "blue"))
                                 ("KILL" . "red")
                                 ("CANCELLED" . (:foreground "red" :weight "bold"))
                                 ))
  (setq org-capture-templates '(
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
  (setq org-roam-directory (file-truename "~/org"))    ; Set org-roam directory
  (setq org-roam-v2-ack t)                              ; Disable Warning for org-roam v2
  (org-roam-setup)                                      ; Setup Org-Roam
  )

(map! :leader :desc "Toggle org-roam Buffer" :n "n r l" #'org-roam-buffer-toggle)
(map! :leader :desc "Find org-roam Node" :n "n r f" #'org-roam-node-find)
(map! :leader :desc "Insert org-roam Node" :n "n r i" #'org-roam-node-insert)
(map! :leader :desc "Complete org-roam " :n "n r c" #'org-roam-node-insert)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


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
