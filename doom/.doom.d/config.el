;;; config.el -*- lexical-binding: t; -*-
;;; In case you run in trouble:
;;(toggle-debug-on-error)
;; This Configuration File is managed by ~/Emacs.org. See additional comments there.

(setq user-full-name "Jonathan Pieper"
      user-mail-address "jpieper@mailbox.org"
      calendar-longitude +8.8   ; 8.8  East
      calendar-latitude  +50.1  ; 50.1 Nord
      epg-user-id "2361DFC839413E7A84B2152B01B6FB927AAEC59B")

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun jp/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'jp/display-startup-time)

(require 'server)
(if (not (server-running-p))
    (server-start))  ; Start Emacs as Server!

(setq-default
 delete-by-moving-to-trash t        ; Delete files to trash
 mouse-yank-at-point t              ; Yank at point rather than pointer
 window-combination-resize t)       ; take new window space from all other windows (not just current)
(setq tab-width 2                   ; Smaller width for tab characters
      undo-limit 80000000           ; Raise undo-limit to 80Mb
      indent-tabs-mode nil          ; Do not use tabs to indent lines
      scroll-margin 2               ; Add a margin when scrolling vertically
      x-stretch-cursor t)           ; Stretch cursor to the glyph width
(set-default-coding-systems 'utf-8) ; Default to utf-8 encoding

;;;; backups
(setq backup-by-copying t
      version-control t
      vc-make-backup-files t
      delete-old-versions 0
      auto-save-include-big-deletions t
      backup-directory-alist `((".*" . ,(concat (or (getenv "XDG_CACHE_HOME") doom-cache-dir) "/emacs/backups")))
      auto-save-file-name-transforms `((".*" ,(concat (or (getenv "XDG_CACHE_HOME") doom-cache-dir) "/emacs/autosaves") t)))


;; World Clock
(setq world-clock-list '(("UTC" "Universal")
                         ("America/Los_Angeles" "Seattle")
                         ("America/Chicago" "Chicago")
                         ("America/New_York" "New York")
                         ("Europe/London" "London")
                         ("Europe/Paris" "Paris")
                         ("Europe/Athens" "Athen")
                         ("Asia/Dubai" "Dubai")
                         ("Asia/Calcutta" "Calcutta")
                         ("Asia/Bangkok" "Bangkok")
                         ("Asia/Singapore" "Singapur")
                         ("Australia/Perth" "Perth")
                         ("Asia/Tokyo" "Tokyo")
                         ("Australia/Sydney" "Sydney")))

(defvar jp/guix? (if (getenv "GUIX_LOCPATH") t nil)) ; Are we running GNU/GUIX?

;; Frame Transparency
(defun jp/toggle-window-transparency ()
  "Toggle transparency."
  (interactive)
  (let ((alpha-transparency 85))
    (if (eq (frame-parameter nil 'alpha-background) alpha-transparency)
        (set-frame-parameter nil 'alpha-background 100)
      (set-frame-parameter nil 'alpha-background alpha-transparency))))
(add-hook 'emacs-startup-hook #'jp/toggle-window-transparency)

(if (file-directory-p (file-truename "~/.config/doom"))
  (add-to-list 'load-path (file-truename "~/.config/doom"))
  (add-to-list 'load-path (file-truename "~/.doom.d")))
(require 'org-workflow)
(setq org-logseq-dir "~/ZK/logseq")

(map! :leader
      (:prefix ("b" . "buffer")
       :desc "Consult buffer" :n "o" #'consult-buffer
       :desc "Consult buffer other window" :n "j" #'consult-buffer-other-window
       :desc "List bookmarks" "L" #'list-bookmarks
       :desc "Save current bookmarks to bookmark file" "w" #'bookmark-save)
      ;; (:prefix-map ("c" . "code"))
      ;; (:prefix-map ("d" . "dired"))
      ;; (:prefix-map ("f" . "file"))
      ;; (:prefix-map ("g" . "git"))
      ;; (:prefix-map ("h" . "help"))
      (:prefix ("i" . "insert")
       :desc "all-the-icons-insert" "a" #'all-the-icons-insert
       :desc "helm-ucs" "8" #'helm-ucs
       )
      ;; (:prefix-map ("m" . "org manage")
      ;;  (:prefix ("a" . "attatch"))
      ;;  (:prefix ("b" . "table"))
      ;;  (:prefix ("c" . "clock"))
      ;;  (:prefix ("d" . "date"))
      ;;  )
      (:prefix ("o" . "open")
       :desc "spotlight" "s" #'spotlight
       (:prefix ("j" . "jp")
        :desc "jp/org-roam-agenda" "a" #'jp/org-roam-agenda
        :desc "jp/enable-bitwarden" "b" #'jp/enable-bitwarden
        :desc "jp/go-to-inbox" "i" #'jp/go-to-inbox
        :desc "jp/go-to-projects" "p" #'jp/go-to-projects
        (:prefix ("r" . "roam")
         :desc "jp/org-roam-agenda" "a" #'jp/org-roam-agenda
         :desc "jp/org-roam-ignore-literature" "L" #'jp/org-roam-ignore-literature
         :desc "jp/org-roam-select-literature" "l" #'jp/org-roam-select-literature
         :desc "jp/org-roam-ignore-other" "O" #'jp/org-roam-ignore-other
         :desc "jp/org-roam-select-other" "o" #'jp/org-roam-select-other
         :desc "jp/org-roam-ignore-projects" "P" #'jp/org-roam-ignore-projects
         :desc "jp/org-roam-select-projects" "p" #'jp/org-roam-select-projects
         (:prefix ("r" . "review")
          :desc "jp/daily-review" "d" #'jp/daily-review
          :desc "jp/monthly-review" "m" #'jp/monthly-review
          :desc "jp/weekly-review" "w" #'jp/weekly-review
          )
         :desc "jp/org-roam-ignore-pc" "C" #'jp/org-roam-ignore-pc
         :desc "jp/org-roam-select-pc" "c" #'jp/org-roam-select-pc
         )
        )
       )
      ;; (:prefix-map ("p" . "projectile"))
      ;; (:prefix-map ("q" . "quit"))
      (:prefix ("s" . "search")
       :desc "counsel ag" "a" #'counsel-ag
       :desc "helm ag" "A" #'helm-ag
       :desc "Search/Insert BibTeX Cite" "c" #'org-ref-cite-insert-helm
       :desc "Consult Ripgrep" :n "R" #'consult-ripgrep
       (:prefix ("g" . "GNU/Guix")
        :desc "All Packages" "ap" #'guix-all-packages
        :desc "All Services" "as" #'guix-all-services
        :desc "Guix Command" "c" #'guix-command
        :desc "Guix (Popup)" "g" #'guix-popup
        :desc "Lint" "L" #'guix-lint
        :desc "Find License Definition" "l" #'guix-find-license-definition
        :desc "Find Package Definition" "p" #'guix-find-package-definition
        :desc "Find Service Definition" "s" #'guix-find-service-definition
        )
       )
      (:prefix ("t" . "toggle")
       :desc "Toggle alpha/transparency" "a" #'jp/toggle-window-transparency
       :desc "Toggle global debug on error" "d" #'toggle-debug-on-error
       :desc "Org Present" "p"  #'org-present
       :desc "Toggle line highlight local" "h" #'hl-line-mode
       :desc "Toggle line highlight globally" "H" #'global-hl-line-mode
       :desc "Toggle KeyCast Mode" "k" #'keycast-mode
       :desc "Toggle Menu Bar" "m" #'menu-bar-mode
       :desc "Toggle writegood mode" "S" #'writegood-mode
       :desc "Toggle truncate lines" "t" #'toggle-truncate-lines
       :desc "Toggle highlight TODOs" "T" #'hl-todo-mode
       :desc "Toggle visual fill column" "v" #'visual-fill-column-mode
       (:prefix ("SPC" . "Whitespaces")
        :desc "Toggle local whitespace option" "l" #'whitespace-toggle-options
        :desc "Toggle global whitespace option" "g" #'global-whitespace-toggle-options
        :desc "Toggle local whitespace mode" "t" #'whitespace-mode
        :desc "Toggle global whitespace mode" "w" #'global-whitespace-mode
        )
       )
      (:prefix ("w" . "window")
       :desc "hydra" "e" #'jp/hydra/windows/body
       :desc "evil-window-left" :n "<left>" #'evil-window-left
       :desc "evil-window-right" :n "<right>" #'evil-window-right
       :desc "evil-window-up" :n "<up>" #'evil-window-up
       :desc "evil-window-down" :n "<down>" #'evil-window-down
       )
      (:prefix ("n" . "notes")
               (:prefix ("r" . "roam")
                :desc "Insert BibTeX Note Link" "b" #'orb-insert-link
                :desc "BibTeX Note Actions" "B" #'orb-note-actions
                :desc "Complete org-roam " :n "c" #'org-roam-complete-at-point
                :desc "Delve" :n "D" #'delve
                :desc "New Daily Node (today)" :n "t" #'org-roam-dailies-capture-today
                :desc "Find org-roam Node" :n "F" #'org-roam-node-find
                :desc "Find no priv Node" :n "f" #'jp/org-roam-ignore-priv
                :desc "Find no acg Node" :n "q" #'jp/org-roam-ignore-acg
                :desc "Insert org-roam Node" :n "i" #'org-roam-node-insert
                :desc "Capture new org-roam Node" :n "n" #'org-roam-capture
                :desc "Org Roam UI" :n "u" #'org-roam-ui-open
                :desc "Jump to Date" :n "j" #'jp/org-roam-jump-menu/body
                )
               )
      ;; (:prefix-map ("TAB" . "workspace"))
      )

(map! :leader
      (:prefix ("e". "evaluate/EWW")
       :desc "Evaluate elisp in buffer" :n "b" #'eval-buffer
       :desc "Evaluate defun" :n "d" #'eval-defun
       :desc "Evaluate elisp expression" :n "e" #'eval-expression
       :desc "Evaluate last sexpression" :n "l" #'eval-last-sexp
       :desc "Evaluate elisp in region" :n "r" #'eval-region))

(map! :leader
      (:prefix ("l" . "lookup")
       (:prefix ("p" . "passwords")
        :desc "list-all" "a" #'bitwarden-list-all
        :desc "jp/enable-bitwarden" "e" #'jp/enable-bitwarden
        :desc "getpass" "g" #'bitwarden-getpass
        :desc "login" "l" #'bitwarden-login
        :desc "unlock" "u" #'bitwarden-unlock
        :desc "remove from kill-ring" "x" #'embark-kill-ring-remove
                 )
       :desc "helm-M-x" "c" #'helm-M-x
       :desc "helm-bibtex" "b" #'helm-bibtex
       :desc "counsel-fonts" "f" #'counsel-fonts
       :desc "helm-occur" "o" #'helm-occur
       :desc "helm-imenu" "i" #'helm-imenu
       :desc "helm-imenu-in-all-buffers" "I" #'helm-imenu-in-all-buffers
       :desc "helm-regexp" "r" #'helm-regexp
       :desc "helm-ucs" "S" #'helm-ucs
       :desc "helm-top" "T" #'helm-top
       :desc "helm-tldr" "t" #'helm-tldr
       :desc "helm-man-woman" "m" #'helm-man-woman
       )
      )

(map! :leader
      (:prefix ("j" . "jump")
       :desc "avy-goto-char" "c" #'avy-goto-char
       :desc "avy-goto-char-timer" "o" #'avy-goto-char-timer
       :desc "avy-goto-char-2" "O" #'avy-goto-char-2
       :desc "avy-imenu" "I" #'avy-imenu
       :desc "evil-avy-goto-line" "l" #'evil-avy-goto-line
       :desc "helm-mark-ring" "m" #'helm-mark-ring
       :desc "pomm" "p" #'pomm
       :desc "evil-avy-goto-word-0" "w" #'evil-avy-goto-word-0
       :desc "evil-avy-goto-subword-0" "W" #'evil-avy-goto-subword-0
       )
      )

(define-key bibtex-mode-map (kbd "M-b") 'org-ref-bibtex-hydra/body)
(define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link-hydra/body)

(require 'embark)
(global-set-key (kbd "C-@") 'embark-act)

(eval-when-compile
  (defmacro my/embark-ace-action (fn)
    `(defun ,(intern (concat "my/embark-ace-" (symbol-name fn))) ()
       (interactive)
       (with-demoted-errors "%s"
         (require 'ace-window)
         (aw-switch-to-window (aw-select nil))
         (call-interactively (symbol-function ',fn)))))

  (defmacro my/embark-split-action (fn split-type)
    `(defun ,(intern (concat "my/embark-"
                             (symbol-name fn)
                             "-"
                             (car (last  (split-string
                                          (symbol-name split-type) "-"))))) ()
       (interactive)
       (funcall #',split-type)
       (call-interactively #',fn))))

(define-key embark-file-map     (kbd "o") (my/embark-ace-action find-file))
(define-key embark-buffer-map   (kbd "o") (my/embark-ace-action switch-to-buffer))
(define-key embark-bookmark-map (kbd "o") (my/embark-ace-action bookmark-jump))

(define-key embark-file-map     (kbd "2") (my/embark-split-action find-file split-window-below))
(define-key embark-buffer-map   (kbd "2") (my/embark-split-action switch-to-buffer split-window-below))
(define-key embark-bookmark-map (kbd "2") (my/embark-split-action bookmark-jump split-window-below))

(define-key embark-file-map     (kbd "3") (my/embark-split-action find-file split-window-right))
(define-key embark-buffer-map   (kbd "3") (my/embark-split-action switch-to-buffer split-window-right))
(define-key embark-bookmark-map (kbd "3") (my/embark-split-action bookmark-jump split-window-right))

(map! :map evil-window-map
      "SPC" #'rotate-layout
      "e" #'jp/hydra/windows/body
      ;; Navigation
      "<left>"     #'evil-window-left
      "<down>"     #'evil-window-down
      "<up>"       #'evil-window-up
      "<right>"    #'evil-window-right
      "H-<left>"     #'evil-window-left
      "H-<down>"     #'evil-window-down
      "H-<up>"       #'evil-window-up
      "H-<right>"    #'evil-window-right
      ;; Swapping windows
      "C-<left>"       #'+evil/window-move-left
      "C-<down>"       #'+evil/window-move-down
      "C-<up>"         #'+evil/window-move-up
      "C-<right>"      #'+evil/window-move-right
      )

(unbind-key "K" evil-normal-state-map)
(unbind-key "K" evil-visual-state-map)
(map! :nv "gK"  #'+lookup/documentation)

(progn
  (define-key key-translation-map (kbd "H-<next>") (kbd "<next>"))
  (define-key key-translation-map (kbd "H-<prior>") (kbd "<prior>"))
  (define-key key-translation-map (kbd "H-<end>") (kbd "<end>"))
  (define-key key-translation-map (kbd "H-<home>") (kbd "<home>"))
  (define-key key-translation-map (kbd "H-<escape>") (kbd "<escape>"))
  (define-key key-translation-map (kbd "H-ü") (kbd "<escape>"))
  (define-key key-translation-map (kbd "H-<left>") (kbd "<left>"))
  (define-key key-translation-map (kbd "H-<right>") (kbd "<right>"))
  (define-key key-translation-map (kbd "H-<up>") (kbd "<up>"))
  (define-key key-translation-map (kbd "H-<down>") (kbd "<down>"))
  (define-key key-translation-map (kbd "H-<backspace>") (kbd "<DEL>"))
  (define-key key-translation-map (kbd "H-<delete>") (kbd "<deletechar>"))
  (define-key key-translation-map (kbd "H-<return>") (kbd "<RET>"))
  (dolist (i '(0 1 2 3 4 5 6 7 8 9))
    (define-key key-translation-map
                (kbd (format "H-<kp-%d>" i)) (kbd (number-to-string i)))))

(map! "H-¿" #'counsel-ag)                               ; H-s
(map! "H-<kp-add>" #'jp/org-roam-jump-menu/body)        ; H-q
(map! "H-¡" #'+hydra/window-nav/body)                   ; H-k
(map! "H-;" #'hydra-ivy/body)                           ; H-j
(map! "H-<insert>" #'ivy-mode)                          ; H-ä
(map! "H-<tab>" #'org-agenda)                           ; H-ö
(map! "H-<kp-separator>" #'embark-act)                  ; H-d
(map! "H-<undo>" #'jp/org-roam-refresh-agenda-list)     ; H-z

;; (let ((theme-carusell '(
;;                         doom-outrun-electric
;;                         modus-vivendi
;;                         ef-bio
;;                         ef-dark
;;                         ef-deuteranopia-dark
;;                         ef-duo-dark
;;                         ef-night
;;                         ;; ef-autumn
;;                         ;; ef-trio-dark
;;                         ;; ef-tritanopia-dark
;;                         ef-winter)))
(setq doom-theme 'modus-vivendi)

(setq doom-font (font-spec :family "JetBrains Mono" :size 14)
      doom-big-font (font-spec :family "JetBrains Mono" :size 22)
      doom-variable-pitch-font (font-spec :family "Overpass" :size 16)
      doom-unicode-font (font-spec :family "JetBrains Mono" :size 14)
      doom-serif-font (font-spec :family "JetBrains Mono" :weight 'light :size 14))

(setq display-line-numbers-type 'relative)

;; Disable Line Numbers for specific modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(defun jp/more-whitespaces ()
  (interactive)
  ;; Make whitespace-mode with very basic background coloring for whitespaces.
  (setq whitespace-style (quote (face spaces tabs newline space-mark tab-mark newline-mark)))

  ;; Make whitespace-mode and whitespace-newline-mode use “¶” for end of line char and “⇥” for tab.
  (setq whitespace-display-mappings
        ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
        '(
          ;; (space-mark 32 [183] [46]) ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」, 2FD MODIFIER LETTER SHELF 「˽」
          (newline-mark 10 [182 10]) ; LINE FEED ¶
          (tab-mark 9 [8677 9] [92 9]) ; tab ⇥
          ))
  (whitespace-mode 1))

;; Set up the visible bell
(setq visible-bell t)

(setq alert-default-style 'osx-notifier)

(menu-bar-mode -1)

(map! :n [mouse-8] #'better-jumper-jump-backward
      :n [mouse-9] #'better-jumper-jump-forward)

(setq tab-bar-close-button-show nil
      tab-bar-new-button-show nil)

(setq confirm-kill-emacs nil)           ;; Don't confirm every kill

(setq
 evil-want-fine-undo t                  ;; Undo Emacs Style. By default while in insert all changes are one big blob.
 evil-vsplit-window-right t             ;; Split windows the other way around
 evil-split-window-below t)

(use-package! doom-modeline
  :custom-face
  (mode-line ((t (:height 1.0))))
  (mode-line-inactive ((t (:height 0.95))))
  :custom
  (doom-modeline-height 16)
  (doom-modeline-bar-width 4)
  (doom-modeline-lsp t)
  (doom-modeline-display-default-persp-name t)
  (doom-modeline-modal-icon t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-major-mode-icon t)

  (defun doom-modeline-conditional-buffer-encoding ()
    "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
    (setq-local doom-modeline-buffer-encoding
                (unless (and (memq (plist-get (coding-system-plist buffer-file-coding-system) :category)
                                   '(coding-category-undecided coding-category-utf-8))
                             (not (memq (coding-system-eol-type buffer-file-coding-system) '(1 2))))
                  t)))

  (add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding) (doom-modeline-buffer-state-icon t))

(setq display-time-24hr-format t                ;; Display 24 Hrs rather than 12
      display-time-default-load-average nil)    ;; Do not display my CPU Load
(display-time-mode 0)

(map! :map +doom-dashboard-mode-map
      :ne "f" #'find-file
      :ne "r" #'consult-recent-file
      :ne "p" #'jp/go-to-projects
      :ne "c" #'jp/go-to-config
      :ne "i" #'jp/go-to-inbox
      :ne "." (cmd! (doom-project-find-file "~/.config/")) ; . for dotfiles
      :desc "Notes (roam)" :ne "n" #'org-roam-node-find
      :desc "Open dotfile" :ne "d" (cmd! (doom-project-find-file "~/.dotfiles/"))
      :desc "IBuffer" :ne "i" #'ibuffer
      :desc "ivy-mode" :ne "I" #'ivy-mode
      :desc "Previous buffer" :ne "p" #'previous-buffer
      :desc "Set theme" :ne "t" #'consult-theme
      :ne "b" #'+vertico/switch-workspace-buffer
      :ne "B" #'counsel-switch-buffer)

(setq +doom-dashboard-menu-sections '(("Reload last session" :icon
                                       (all-the-icons-octicon "history" :face 'doom-dashboard-menu-title)
                                       :when
                                       (cond
                                        ((featurep! :ui workspaces)
                                         (file-exists-p
                                          (expand-file-name persp-auto-save-fname persp-save-dir)))
                                        ((require 'desktop nil t)
                                         (file-exists-p
                                          (desktop-full-file-name))))
                                       :face
                                       (:inherit
                                        (doom-dashboard-menu-title bold))
                                       :action doom/quickload-session)
                                      ("Open org-agenda" :icon
                                       (all-the-icons-octicon "calendar" :face 'doom-dashboard-menu-title)
                                       :action org-agenda)
                                      ("Open Roam Notes" :icon
                                       (all-the-icons-octicon "search"
                                                              :face 'doom-dashboard-menu-title)
                                       :action org-roam-node-find)
                                      ("Open IBuffer" :icon
                                       (all-the-icons-octicon "list-unordered"
                                                              :face 'doom-dashboard-menu-title)
                                       :action ibuffer)
                                      ("Refresh Agenda Files" :icon
                                       (all-the-icons-octicon "database"
                                                              :face 'doom-dashboard-menu-title)
                                       :action jp/org-roam-refresh-agenda-list)
                                      ("Recently opened files" :icon
                                       (all-the-icons-octicon "file-text" :face 'doom-dashboard-menu-title)
                                       :action recentf-open-files)
                                      ("Open project" :icon
                                       (all-the-icons-octicon "briefcase" :face 'doom-dashboard-menu-title)
                                       :action projectile-switch-project)
                                      ("Jump to bookmark" :icon
                                       (all-the-icons-octicon "bookmark" :face 'doom-dashboard-menu-title)
                                       :action bookmark-jump)
                                      ("Open private configuration" :icon
                                       (all-the-icons-octicon "tools" :face 'doom-dashboard-menu-title)
                                       :when
                                       (file-directory-p doom-private-dir)
                                       :action doom/open-private-config)
                                      ("Switch Workspace Buffer" :icon
                                       (all-the-icons-octicon "file-symlink-file" :face 'doom-dashboard-menu-title)
                                       :action +vertico/switch-workspace-buffer)
                                      ("Switch Buffer" :icon
                                       (all-the-icons-octicon "file-symlink-directory" :face 'doom-dashboard-menu-title)
                                       :action counsel-switch-buffer)))

(setq hl-todo-keyword-faces
      '(("TODO"   . "#cc00cc")     ;; TODO
        ("FIXME"  . "#990000")    ;; FIXME
        ("NOTE"   . "#009999")    ;; NOTE
        ("REVIEW" . "#990099")    ;; REVIEW
        ("DEBUG"  . "#A020F0")    ;; DEBUG
        ("HACK"   . "#ff6600")       ;; HACK
        ("GOTCHA" . "#FF4500")    ;; GOTCHA
        ("STUB"   . "#1E90FF")))   ;; STUB

(hl-todo-mode)          ; Enable highlight todos

(setq ispell-program-name (executable-find "aspell"))

(pdf-tools-install)

;; Fit PDF in screen width
;; (setq pdf-view-display-size 'fit-width)

;; Show PDF in current Theme Colors
;; (add-hook 'pdf-view-mode-hook (lambda() (pdf-view-themed-minor-mode)))

;; Cut off unwritten borders of PDF.
;; (add-hook 'pdf-view-mode-hook (lambda() (pdf-view-auto-slice-minor-mode)))

;; Open .epub with nov.el package
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; Set custom font for epub
(defun my-nov-font-setup ()
  (face-remap-add-relative 'variable-pitch :family "Roboto"
                           :height 1.0))
(add-hook 'nov-mode-hook 'my-nov-font-setup)

(after! keycast
  (require 'keycast)
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
        (add-hook 'pre-command-hook 'keycast--update t)
      (remove-hook 'pre-command-hook 'keycast--update))))

(add-to-list 'global-mode-string '("" keycast-mode-line))

;;(setq avy-keys '(97 115 100 102 103 104 106 107 108))
(setq avy-keys '(?u ?i ?a ?e ?o ?s ?n ?r ?t))

(require 'avy)
(defun avy-action-mark-to-char (pt)
  (activate-mark)
  (goto-char pt))

(setf (alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char)

(defun avy-action-copy-whole-line (pt)
  (save-excursion
    (goto-char pt)
    (cl-destructuring-bind (start . end)
        (bounds-of-thing-at-point 'line)
      (copy-region-as-kill start end)))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

(defun avy-action-yank-whole-line (pt)
  (avy-action-copy-whole-line pt)
  (save-excursion (yank))
  t)

(defun avy-action-kill-whole-line (pt)
  (save-excursion
    (goto-char pt)
    (kill-whole-line))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

(setf (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay
      (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line
      (alist-get ?y avy-dispatch-alist) 'avy-action-yank
      (alist-get ?w avy-dispatch-alist) 'avy-action-copy
      (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line
      (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line)

(defun avy-action-teleport-whole-line (pt)
  (avy-action-kill-whole-line pt)
  (save-excursion (yank)) t)

(setf (alist-get ?m avy-dispatch-alist) 'avy-action-teleport
      (alist-get ?M avy-dispatch-alist) 'avy-action-teleport-whole-line)

(defun dictionary-search-dwim (&optional arg)
  "Search for definition of word at point. If region is active,
search for contents of region instead. If called with a prefix
argument, query for word to search."
  (interactive "P")
  (if arg
      (dictionary-search nil)
    (if (use-region-p)
        (dictionary-search (buffer-substring-no-properties
                            (region-beginning)
                            (region-end)))
      (if (thing-at-point 'word)
          (dictionary-lookup-definition)
        (dictionary-search-dwim '(4))))))

(defun avy-action-define (pt)
  (save-excursion
    (goto-char pt)
    (dictionary-search-dwim))
  (select-window
   (cdr (ring-ref avy-ring 0)))
  t)

(setf (alist-get ?= avy-dispatch-alist) 'avy-action-define)

(defun avy-action-helpful (pt)
  (save-excursion
    (goto-char pt)
    (helpful-at-point))
  (select-window
   (cdr (ring-ref avy-ring 0)))
  t)

(setf (alist-get ?H avy-dispatch-alist) 'avy-action-helpful)

(defun avy-action-embark (pt)
  (save-excursion
    (goto-char pt)
    (embark-act))
  (select-window
   (cdr (ring-ref avy-ring 0)))
  t)

(setf (alist-get ?x avy-dispatch-alist) 'avy-action-embark)

(defhydra jp/hydra/windows (:hint nil)
  "
          Split: _v_ert  _s_:horz
         Delete: _c_lose  _o_nly
  Switch Window: _h_:left  _j_:down  _k_:up  _l_:right  _u_:undo  _r_:redo
        Buffers: _p_revious  _n_ext  _b_:select  _f_ind-file
         Resize: _H_:splitter left  _J_:splitter down  _K_:splitter up  _L_:splitter right
           Move: _a_:up  _z_:down  _i_menu
"
  ("z" scroll-up-line)
  ("a" scroll-down-line)
  ("i" consult-imenu)

  ("h" windmove-left)
  ("<left>" windmove-left)
  ("j" windmove-down)
  ("<down>" windmove-down)
  ("k" windmove-up)
  ("<up>" windmove-up)
  ("l" windmove-right)
  ("<right>" windmove-right)
  ("u" winner-undo)
  ("r" winner-redo)

  ("p" previous-buffer)
  ("n" next-buffer)
  ("b" switch-to-buffer)
  ("f" find-file)

  ("s" split-window-below)
  ("v" split-window-right)

  ("c" delete-window)
  ("o" delete-other-windows)

  ("H" hydra-move-splitter-left)
  ("J" hydra-move-splitter-down)
  ("K" hydra-move-splitter-up)
  ("L" hydra-move-splitter-right)

  ("q" nil))

(after! popper
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (global-set-key (kbd "C-`") 'popper-toggle-latest)
  (global-set-key (kbd "M-`") 'popper-cycle)
  (global-set-key (kbd "C-M-`") 'popper-toggle-type)
  (popper-mode +1))

(after! guix
  (global-guix-prettify-mode 1))

(setq undo-tree-visualizer-timestamps t) ; Display Timestamps

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

(map! :leader
      (:prefix ("d" . "dired")
       :desc "Open dired" "d" #'dired
       :desc "Dired jump to current" "j" #'dired-jump)
      (:after dired
       (:map dired-mode-map
        :desc "Peep-dired image previews" "d p" #'peep-dired
        :desc "Dired view file" "d v" #'dired-view-file)))

(defun jp/dired-hide-dotfiles()
    (setq dired-omit-files
          (rx (or (seq bol (? ".") "#")
                  (seq bol "." eol)
                  (seq bol ".." eol)
                  )))
    )

;; Make 'h' and 'l' go back and forward in dired. Much faster to navigate the directory structure!
(evil-define-key 'normal dired-mode-map
  (kbd "M-RET") 'dired-display-file
  (kbd "h") 'dired-up-directory
;;  (kbd "l") 'dired-open-file ; use dired-find-file instead of dired-open.
  (kbd "m") 'dired-mark
  (kbd "t") 'dired-toggle-marks
  (kbd "u") 'dired-unmark
  (kbd "C") 'dired-do-copy
  (kbd "D") 'dired-do-delete
;;  (kbd "H") #'jp/dired-hide-dotfiles
  (kbd "J") 'dired-goto-file
  (kbd "M") 'dired-do-chmod
  (kbd "O") 'dired-do-chown
  (kbd "P") 'dired-do-print
  (kbd "R") 'dired-do-rename
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

  (defun jp/lsp-mode-setup ()
    (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
    (lsp-headerline-breadcrumb-mode))

  (use-package lsp-mode
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

(defun jp/python-mode-hook()
  (require 'lsp-pyright)
  (require 'dap-python)
  (modify-syntax-entry ?_ "w") ; treat underscore (_) as word-breaking character
  (lsp-deferred))

(add-hook 'python-mode-hook #'jp/python-mode-hook)

;; NOTE: Set these if Python 3 is called "python3" on your system!
(setq dap-python-debugger 'debugpy)

(defvar jp/guix/pythonpath (getenv "GUIX_PYTHONPATH")
  "Absolute Python Library Path (e.g. /usr/share/lib/python3.9/site-packages)")
(defvar jp/conda/pythonpath (getenv "CONDA_PYTHON_EXE")
  "Absolute Conda Python Exe Path (e.g. /opt/miniconda3/bin/python)")
(defvar jp/conda/prefix (getenv "CONDA_PREFIX")
  "Absolute Path to current Conda Prefix (e.g. /home/user/.conda/envs/my-environment)")
(defvar jp/python
  (if jp/guix/pythonpath
      (concat (ivy--parent-dir (ivy--parent-dir (ivy--parent-dir jp/guix/pythonpath))) "bin/python3")
    (if jp/conda/prefix
        (concat jp/conda/prefix "/bin/python")
      (if jp/conda/pythonpath
          jp/conda/pythonpath
        "/opt/homebrew/conda/bin/python"
        )))
  "Python binary path.")
(setq python-shell-interpreter jp/python
      dap-python-executable jp/python
      treemacs-python-executable jp/python
      lsp-pyright-python-executable-cmd jp/python)

;; Anaconda Path
(setq conda-env-home-directory "/opt/homebrew/Caskroom/miniforge/base"
      conda-anaconda-home conda-env-home-directory)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
   ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

;; NOTE: Set this to the folder where you keep your Git repos!
(when (file-directory-p "~/Projects/Code")
  (setq projectile-project-search-path '("~/Projects/Code")))
(setq projectile-switch-project-action #'projectile-dired)

(setq projectile-completion-system 'vertico)

  (defun jp/configure-eshell ()
    ;; Save command history when commands are entered
    (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

    ;; Truncate buffer for performance
    (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

    ;; Bind some useful keys for evil-mode
    (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
    (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
    (evil-normalize-keymaps)

    (setq eshell-history-size         10000
          eshell-buffer-maximum-lines 10000
          eshell-hist-ignoredups t
          eshell-scroll-to-bottom-on-input t))

  (use-package eshell-git-prompt
    :after eshell)

  (use-package eshell
    :hook (eshell-first-time-mode . jp/configure-eshell)
    :config

    (with-eval-after-load 'esh-opt
      (setq eshell-destroy-buffer-when-process-dies t)
      (setq eshell-visual-commands '("htop" "zsh" "vim")))

    (eshell-git-prompt-use-theme 'powerline))

(after! magit
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  )

;; Magit Configuration to enable gpg to sign keys
(unless jp/guix?
  (progn
    (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
    (setq exec-path (append exec-path '("/usr/local/bin")))))

;; Tell Emacs where to find mu4e (only necessary if manual compiled)
(pcase system-type
  ((or 'gnu/linux 'windows-nt 'cygwin)
   (unless jp/guix?
     (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")))
  ('darwin
   (use-package mu4e
     :load-path  "/opt/homebrew/share/emacs/site-lisp/mu/mu4e/")))

;; Load org-mode integration
;;(require 'mu4e-org)

(after! mu4e
  ;; Refresh mail using isync every 10 minutes
  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "~/Mail")

  ;; Use Ivy for mu4e completions (maildir folders, etc)
  ;;(setq mu4e-completing-read-function #'ivy-completing-read)

  ;; Make sure that moving a message (like to Trash) causes the
  ;; message to get a new file name.  This helps to avoid the
  ;; dreaded "UID is N beyond highest assigned" error.
  ;; See this link for more info: https://stackoverflow.com/a/43461973
  (setq mu4e-change-filenames-when-moving t)

  ;; Make sure to use 24h time format.
  (setq mu4e-headers-time-format "%T" ; %T: Full 24h-Time [same as %H:%M:%S] (e.g. 23:59:59)
        mu4e-headers-date-format "%d/%m/%y"
        ;;mu4e-view-date-format "%F %T" ; %F: Full date [like %+4Y-%m-%d] (e.g. 2021-12-31)
        ;;mu4e-date-format-long "%F %T"
        ;;mu4e-headers-long-date-format "%F %T"
        )

  ;; Set up contexts for email accounts
  (setq mu4e-contexts
        `(,(make-mu4e-context
            :name "Mailbox"
            :match-func (lambda (msg) (when msg
                                        (string-prefix-p "/Mailbox" (mu4e-message-field msg :maildir))))
            :vars '(
                    (user-full-name . "Jonathan Pieper")
                    (user-mail-address . "jpieper@mailbox.org")
                    (mu4e-sent-folder . "/Mailbox/Sent")
                    (mu4e-trash-folder . "/Mailbox/Trash")
                    (mu4e-drafts-folder . "/Mailbox/Drafts")
                    (mu4e-refile-folder . "/Mailbox/Archives")
                    (mu4e-sent-messages-behavior . sent)
                    ))
          ,(make-mu4e-context
            :name "Personal"
            :match-func (lambda (msg) (when msg
                                        (string-prefix-p "/Personal" (mu4e-message-field msg :maildir))))
            :vars '(
                    (mu4e-sent-folder . "/Personal/Sent")
                    (mu4e-trash-folder . "/Personal/Deleted")
                    (mu4e-refile-folder . "/Personal/Archive")
                    ))
          ))
  (setq mu4e-context-policy 'pick-first)

  ;; Prevent mu4e from permanently deleting trashed items
  ;; This snippet was taken from the following article:
  ;; http://cachestocaches.com/2017/3/complete-guide-email-emacs-using-mu-and-/
  (defun remove-nth-element (nth list)
    (if (zerop nth) (cdr list)
      (let ((last (nthcdr (1- nth) list)))
        (setcdr last (cddr last))
        list)))

  ;; Display options
  (setq mu4e-view-show-images t)
  (setq mu4e-view-show-addresses 't)

  ;; Composing mail
  (setq mu4e-compose-dont-reply-to-self t)

  ;; Use mu4e for sending e-mail
  (setq mail-user-agent 'mu4e-user-agent
        message-send-mail-function 'smtpmail-send-it
        smtpmail-smtp-server "smtp.mailbox.org"
        smtpmail-smtp-service 465
        smtpmail-stream-type  'ssl)

  ;; Signing messages (use mml-secure-sign-pgpmime)
  (setq mml-secure-openpgp-signers '("2361DFC839413E7A84B2152B01B6FB927AAEC59B"))
  (defun sign-or-encrypt-message ()
    (let ((answer (read-from-minibuffer "Sign or encrypt?\nEmpty to do nothing.\n[s/e]: ")))
      (cond
       ((string-equal answer "s") (progn
                                    (message "Signing message.")
                                    (mml-secure-message-sign-pgpmime)))
       ((string-equal answer "e") (progn
                                    (message "Encrypt and signing message.")
                                    (mml-secure-message-encrypt-pgpmime)))
       (t (progn
            (message "Dont signing or encrypting message.")
            nil)))))

  (add-hook 'message-send-hook 'sign-or-encrypt-message)

  ;; (See the documentation for `mu4e-sent-messages-behavior' if you have
  ;; additional non-Gmail addresses and want assign them different
  ;; behavior.)

  ;; setup some handy shortcuts
  ;; you can quickly switch to your Inbox -- press ``ji''
  ;; then, when you want archive some messages, move them to
  ;; the 'All Mail' folder by pressing ``ma''.
  (setq mu4e-maildir-shortcuts
        '(("/Mailbox/INBOX"       . ?i)
          ("/Mailbox/INBOX/*"     . ?l)
          ("/Mailbox/Sent"        . ?s)
          ("/Mailbox/Trash"       . ?t)))

  ;; (add-to-list 'mu4e-bookmarks
  (mu4e-bookmark-define
    "All Inboxes"
    "maildir:/Mailbox/INBOX OR maildir:/Personal/Inbox AND NOT flag:trashed"
    ?i)

  ;; (add-to-list 'mu4e-bookmarks
  (mu4e-bookmark-define
    "from:/.*@uni-frankfurt/ OR maildir:/Personal/Uni"
    "Uni-Frankfurt"
    ?g)

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)

  (setq jp/mu4e-inbox-query
        "(maildir:/Personal/Inbox OR maildir:/Mailbox/INBOX) AND flag:unread AND NOT flag:trashed")

  (defun jp/go-to-inbox ()
    (interactive)
    (mu4e-headers-search jp/mu4e-inbox-query)

    (setq mu4e-marks (remove-nth-element 5 mu4e-marks))
    (add-to-list 'mu4e-marks
                 '(trash
                   :char ("d" . "▼")
                   :prompt "dtrash"
                   :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
                   :action (lambda (docid msg target)
                             (mu4e~proc-move docid
                                             (mu4e~mark-check-target target) "-N"))))

    ;; Use [[https://github.com/iqbalansari/mu4e-alert][mu4e-alert]]
    ;; to show notifications when e-mail comes in.
    ;; Show unread emails from all inboxes
    (setq mu4e-alert-interesting-mail-query jp/mu4e-inbox-query)

    ;; Show notifications for mails already notified
    (setq mu4e-alert-notify-repeated-mails nil)

    (mu4e-alert-enable-notifications)))

(setq auth-sources '("~/.authinfo.gpg")
      auth-source-cache-expiry nil)

(defun jp/lookup-password (&rest keys)
  (let ((result (apply #'auth-source-search keys)))
    (if result
        (funcall (plist-get (car result) :secret))
      nil)))

(defun jp/enable-bitwarden ()
  (interactive)
  (setq bitwarden-automatic-unlock
        (let* ((auth-sources '("~/.authinfo.gpg"))
               (matches (auth-source-search :user "jpieper@mailbox.org"
                                            :host "bw.ody5.de"
                                            :require '(:secret)
                                            :max 1))
               (entry (nth 0 matches)))
          (plist-get entry :secret)))
  (bitwarden-auth-source-enable))

(setq deft-directory "~/org")

(setq jp/decrypt-ledger "")
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-ledger))

(setq ledger-reconcile-default-commodity "€")

(setq ledger-reports
 '(("bal"            "gpg --decrypt %(ledger-file) 2>/dev/null | %(binary) -f - bal")
   ("bal this month" "gpg --decrypt %(ledger-file) 2>/dev/null | %(binary) -f - bal -p %(month) -S amount")
   ("bal this year"  "gpg --decrypt %(ledger-file) 2>/dev/null | %(binary) -f - bal -p 'this year'")
   ("net worth"      "gpg --decrypt %(ledger-file) 2>/dev/null | %(binary) -f - bal Assets Liabilities")
   ("reg"        "gpg --decrypt %(ledger-file) 2>/dev/null | %(binary) -f %(ledger-file) reg")
   ("account"        "gpg --decrypt %(ledger-file) 2>/dev/null | %(binary) -f %(ledger-file) reg %(account)")))

(map! :map ledger-reconcile-mode-map
      :ne "q" #'ledger-reconcile-quit
      :ne "a" #'ledger-reconcile-add
      :ne "d" #'ledger-reconcile-delete)
