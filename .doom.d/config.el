(setq user-full-name "Jonathan Pieper")
(setq user-mail-address "ody55eus@mailbox.org")

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun jp/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'jp/display-startup-time)

(map! :leader
      (:prefix-map ("b" . "buffer")
       :desc "Counsel switch buffer" :n "j" #'counsel-switch-buffer
       :desc "Counsel switch buffer other window" :n "I" #'counsel-switch-buffer-other-window)
      (:prefix-map ("c" . "code"))
      (:prefix-map ("d" . "dired"))
      (:prefix-map ("f" . "file"))
      (:prefix-map ("g" . "git"))
      (:prefix-map ("h" . "help"))
      (:prefix-map ("i" . "emoji"))
      (:prefix-map ("m" . "org manage")
       (:prefix ("a" . "attatch"))
       (:prefix ("b" . "table"))
       (:prefix ("c" . "clock"))
       (:prefix ("d" . "date"))
       )
      (:prefix-map ("o" . "open"))
      (:prefix-map ("p" . "projectile"))
      (:prefix-map ("q" . "quit"))
      (:prefix-map ("s" . "search"))
      (:prefix-map ("t" . "toogle"))
      (:prefix-map ("w" . "window"))
      (:prefix-map ("TAB" . "workspace"))
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

(map! (:prefix-map ("C-w" . "window")
       :desc "evil-window-left" :n "<left>" #'evil-window-left
       :desc "evil-window-right" :n "<right>" #'evil-window-right
       :desc "evil-window-up" :n "<up>" #'evil-window-up
       :desc "evil-window-down" :n "<down>" #'evil-window-down
       )
 )

(setq doom-theme 'doom-solarized-dark-high-contrast)

;; Set the font face based on platform
(pcase system-type
  ((or 'gnu/linux 'windows-nt 'cygwin)
   (set-face-attribute 'default nil
                       :font "Source Code Pro"
                       :weight 'regular
                       :height 140))
  ('darwin (set-face-attribute 'default nil :font "Fira Mono" :height 170)))

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil
                    :font "Source Code Pro"
                    :weight 'regular
                    :height 140)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil
                    ;; :font "Cantarell"
                    :font "Ubuntu"
                    :height 185
                    :weight 'regular)

(setq display-line-numbers-type 'relative)

;; Set up the visible bell
(setq visible-bell t)

(menu-bar-mode 1)

(rainbow-mode)

(pdf-tools-install)

(setq org-directory "~/org/"
      org-agenda-files '("~/org/Agenda.org"
                         "~/org/Tasks.org"
                         "~/org/Habits.org"
                         "~/org/Journal.org")
      org-default-notes-file (concat org-directory "/Notes.org"))

(setq org-roam-directory (file-truename "~/org/roam")   ; Set org-roam directory
      org-roam-v2-ack t)                                ; Disable Warning for org-roam v2

(add-hook 'org-mode-hook (lambda () (org-roam-setup))) ; Enable org-roam

(setq org-ellipsis " ▼ ")

(setq org-hide-emphasis-markers t)      ; Hides *strong* /italic/ =highlight= marker

(defun jp/org-visual-fill-column ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(add-hook 'org-mode-hook #'jp/org-visual-fill-column)

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))) ; Enable org-mode bullets

;; setting org headlines
(custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.2))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.1))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
  )

;; Make sure org-indent face is available
(require 'org-indent)

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
(set-face-attribute 'org-drawer nil :inherit 'fixed-pitch :foreground "SkyBlue4")

(add-hook 'org-mode-hook (lambda () (mixed-pitch-mode))) ; Enable mixed fonts (fixed/variable)

(setq org-todo-keyword-faces '(
                         ("EPIC" . (:foreground "DodgerBlue" :weight "bold"))
                         ("PROJ" . "DarkGreen")
                         ("TODO" . org-warning)
                         ("STRT" . "yellow")
                         ("WAIT" . (:foreground "yellow4" :weight "italic"))
                         ("HOLD" . (:foreground "red4"))
                         ("IDEA" . (:foreground "BlueViolet"))
                         ("KILL" . "red")
                         ("CANCELLED" . (:foreground "red3" :weight "bold"))
                         )
      )

(setq org-todo-keywords '(
                          (type "EPIC(e)" "PROJ(p)" "TODO(t)" "STRT(s)"
                                "WAIT(w)" "HOLD(h)" "IDEA(i)" "|"
                                "DONE(d)" "KILL(k)" "CANCELLED(c)")
                          (sequence "BACKLOG(b)" "PLAN(p)" "ACTIVE(a)"
                                    "REVIEW(r)" "WAIT(w@/!)" "HOLD(h)" "|"
                                    "COMPLETED(c)" "CANC(k@)")
                        )
      )

(setq org-capture-templates '(
                              ("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
                               "* TODO %?\n %i\n %a")
                              ("e" "Epic" entry (file+headline "~/org/epic.org" "Epic")
                               "* EPIC %?\n %i\n %a")
                              ("j" "Journal Entries")
                              ("jj" "Journal" entry
                               (file+olp+datetree "~/org/Journal.org")
                               "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
                               ;; ,(jp/read-file-as-string "~/Notes/Templates/Daily.org")
                               :clock-in :clock-resume
                               :empty-lines 1)
                              ("jm" "Meeting" entry
                               (file+olp+datetree "~/org/Journal.org")
                               "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
                               :clock-in :clock-resume
                               :empty-lines 1)
                              )
      )

(setq org-agenda-custom-commands
     '(("d" "Dashboard"
       ((agenda "" ((org-deadline-warning-days 7)))
        (todo "BACKLOG"
          ((org-agenda-overriding-header "Backlog Tasks")))
        (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

      ("t" "All Todo Tasks"
       ((todo "TODO"
          ((org-agenda-overriding-header "Todo Tasks")))))

      ("W" "Work Tasks" tags-todo "+work-email")

      ;; Low-effort next actions
      ("e" tags-todo "+TODO=\"EPIC\"+Effort<15&+Effort>0"
       ((org-agenda-overriding-header "Low Effort Tasks")
        (org-agenda-max-todos 20)
        (org-agenda-files org-agenda-files)))

      ("w" "Workflow Status"
       ((todo "WAIT"
              ((org-agenda-overriding-header "Waiting on External")
               (org-agenda-files org-agenda-files)))
        (todo "REVIEW"
              ((org-agenda-overriding-header "In Review")
               (org-agenda-files org-agenda-files)))
        (todo "PLAN"
              ((org-agenda-overriding-header "In Planning")
               (org-agenda-todo-list-sublevels nil)
               (org-agenda-files org-agenda-files)))
        (todo "BACKLOG"
              ((org-agenda-overriding-header "Project Backlog")
               (org-agenda-todo-list-sublevels nil)
               (org-agenda-files org-agenda-files)))
        (todo "READY"
              ((org-agenda-overriding-header "Ready for Work")
               (org-agenda-files org-agenda-files)))
        (todo "ACTIVE"
              ((org-agenda-overriding-header "Active Projects")
               (org-agenda-files org-agenda-files)))
        (todo "COMPLETED"
              ((org-agenda-overriding-header "Completed Projects")
               (org-agenda-files org-agenda-files)))
        (todo "CANC"
              ((org-agenda-overriding-header "Cancelled Projects")
               (org-agenda-files org-agenda-files)))))))

(setq org-tag-alist
      '((:startgroup)
         ; Put mutually exclusive tags here
         (:endgroup)
         ("@errand" . ?E)
         ("@home" . ?H)
         ("@work" . ?W)
         ("agenda" . ?a)
         ("planning" . ?p)
         ("publish" . ?P)
         ("batch" . ?b)
         ("note" . ?n)
         ("idea" . ?i)))

    (setq org-refile-targets
      '(("Archive.org" :maxlevel . 1)
        ("Tasks.org" :maxlevel . 1)))

    ;; Save Org buffers after refiling!
    (advice-add 'org-refile :after 'org-save-all-org-buffers)

(require 'org-alert)

;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
(add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("go" . "src go"))
(add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
(add-to-list 'org-structure-template-alist '("json" . "src json"))

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

  (defun jp/lsp-mode-setup ()
    (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
    (lsp-headerline-breadcrumb-mode))

  (use-package lsp-mode
    :ensure nil
    :commands (lsp lsp-deferred)
    :hook (lsp-mode . jp/lsp-mode-setup)
    :init
    (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
    :config
    (lsp-enable-which-key-integration t))

  (use-package lsp-ui
    :hook (lsp-mode . lsp-ui-mode)
    :custom
    (lsp-ui-doc-position 'bottom))

  (use-package lsp-treemacs
    :after lsp)

  (use-package lsp-ivy
    :after lsp)

  (use-package dap-mode
    ;; Uncomment the config below if you want all UI panes to be hidden by default!
    ;; :custom
    ;; (lsp-enable-dap-auto-configure nil)
    ;; :config
    ;; (dap-ui-mode 1)
    :commands dap-debug
    :config
    ;; Set up Node debugging
    (require 'dap-node)
    (dap-node-setup) ;; Automatically installs Node debug adapter if needed

    ;; Bind `C-c l d` to `dap-hydra` for easy access
    (general-define-key
      :keymaps 'lsp-mode-map
      :prefix lsp-keymap-prefix
      "d" '(dap-hydra t :wk "debugger")))
