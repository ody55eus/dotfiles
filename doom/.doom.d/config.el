;;; config.el -*- lexical-binding: t; -*-
;;; In case you run in trouble:
;;(toggle-debug-on-error)
;; This Configuration File is managed by ~/Emacs.org. See additional comments there.

(setq user-full-name "Jonathan Pieper"
      user-mail-address "ody55eus@mailbox.org"
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

(server-start)  ; Start Emacs as Server!

(setq-default
 delete-by-moving-to-trash t        ; Delete files to trash
 mouse-yank-at-point t              ; Yank at point rather than pointer
 window-combination-resize t)       ; take new window space from all other windows (not just current)
(setq tab-width 2                   ; Smaller width for tab characters
      scroll-margin 2               ; Add a margin when scrolling vertically
      x-stretch-cursor t)           ; Stretch cursor to the glyph width
(set-default-coding-systems 'utf-8) ; Default to utf-8 encoding

(map! :leader
      (:prefix ("b" . "buffer")
       :desc "Consult buffer" :n "j" #'consult-buffer
       :desc "Consult buffer other window" :n "J" #'consult-buffer-other-window
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
       :desc "Search/Insert BibTeX Cite" "c" #'helm-bibtex
       )
      (:prefix ("t" . "toogle")
       :desc "Toggle line highlight local" "h" #'hl-line-mode
       :desc "Toggle line highlight globally" "H" #'global-hl-line-mode
       :desc "Toggle KeyCast Mode" "k" #'keycast-mode
       :desc "Toggle truncate lines" "t" #'toggle-truncate-lines
       :desc "Toggle visual fill column" "v" #'visual-fill-column-mode
       (:prefix ("SPC" . "Whitespaces")
        :desc "Toggle local whitespace option" "l" #'whitespace-toggle-options
        :desc "Toggle global whitespace option" "g" #'global-whitespace-toggle-options
        :desc "Toggle local whitespace mode" "t" #'whitespace-mode
        :desc "Toggle global whitespace mode" "w" #'global-whitespace-mode
        )
       )
      (:prefix ("w" . "window")
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
        :desc "New Daily Node (today)" :n "t" #'org-roam-dailies-capture-today
        :desc "Find org-roam Node" :n "f" #'org-roam-node-find
        :desc "Insert org-roam Node" :n "i" #'org-roam-node-insert
        :desc "Capture new org-roam Node" :n "n" #'org-roam-capture
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
       :desc "helm-M-x" "c" #'helm-M-x
       :desc "helm-bibtex" "b" #'helm-bibtex
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
       :desc "pomm" "p" #'pomm
       :desc "evil-avy-goto-word-0" "w" #'evil-avy-goto-word-0
       :desc "evil-avy-goto-subword-0" "W" #'evil-avy-goto-subword-0
       )
      )

(require 'embark)
(global-set-key (kbd "C-:") 'embark-act)

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
      ;; Navigation
      "<left>"     #'evil-window-left
      "<down>"     #'evil-window-down
      "<up>"       #'evil-window-up
      "<right>"    #'evil-window-right
      ;; Swapping windows
      "C-<left>"       #'+evil/window-move-left
      "C-<down>"       #'+evil/window-move-down
      "C-<up>"         #'+evil/window-move-up
      "C-<right>"      #'+evil/window-move-right)

(map! :map +doom-dashboard-mode-map
      :ne "f" #'find-file
      :ne "r" #'consult-recent-file
      :ne "p" #'jp/go-to-projects
      :ne "c" #'jp/go-to-config
      :ne "i" #'jp/go-to-inbox
      :ne "." (cmd! (doom-project-find-file "~/.config/")) ; . for dotfiles
      :ne "b" #'+vertico/switch-workspace-buffer
      :ne "B" #'counsel-switch-buffer)

(setq doom-theme 'doom-vibrant)
(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "DarkOrange"))

(setq doom-font (font-spec :family "JetBrains Mono" :size 16)
      doom-big-font (font-spec :family "JetBrains Mono" :size 22)
      doom-variable-pitch-font (font-spec :family "Overpass" :size 24)
      doom-unicode-font (font-spec :family "JuliaMono" :size 16)
      doom-serif-font (font-spec :family "IBM Plex Mono" :weight 'light :size 16))

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
          (space-mark 32 [183] [46]) ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
          (newline-mark 10 [182 10]) ; LINE FEED,
          (tab-mark 9 [8677 9] [92 9]) ; tab
          ))
  (whitespace-mode 1))

;; Set up the visible bell
(setq visible-bell t)

(setq alert-default-style 'osx-notifier)

(menu-bar-mode 1)

(map! :n [mouse-8] #'better-jumper-jump-backward
      :n [mouse-9] #'better-jumper-jump-forward)

(setq confirm-kill-emacs nil)           ;; Don't confirm every kill

(setq
 evil-want-fine-undo t                  ;; Undo Emacs Style
 evil-vsplit-window-right t             ;; Split windows the other way around
 evil-split-window-below t)

(use-package! doom-modeline
  :custom-face
  (mode-line ((t (:height 1.0))))
  (mode-line-inactive ((t (:height 0.95))))
  :custom
  (doom-modeline-height 16)
  (doom-modeline-bar-width 4)
  (doom-modeline-lsp nil)
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

(add-to-list 'mode-line-misc-info '(:eval pomm-current-mode-line-string))
(add-hook 'pomm-on-tick-hook 'pomm-update-mode-line-string)
(add-hook 'pomm-on-tick-hook 'force-mode-line-update)
(add-hook 'pomm-on-status-changed-hook 'pomm-update-mode-line-string)
(add-hook 'pomm-on-status-changed-hook 'force-mode-line-update)

(setq hl-todo-keyword-faces
      '(("TODO"   . "#cc0")
        ("FIXME"  . "#990000")
        ("NOTE"   . "#009999")
        ("REVIEW"   . "#990099")
        ("DEBUG"  . "#A020F0")
        ("HACK"   . "#f60")
        ("GOTCHA" . "#FF4500")
        ("STUB"   . "#1E90FF")))

(hl-todo-mode)          ; Enable highlight todos

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
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
        (add-hook 'pre-command-hook 'keycast--update t)
      (remove-hook 'pre-command-hook 'keycast--update))))
(add-to-list 'global-mode-string '("" mode-line-keycast))

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

(setf (alist-get ?t avy-dispatch-alist) 'avy-action-teleport
      (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line)

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

(setf (alist-get ?o avy-dispatch-alist) 'avy-action-embark)

(after! org
  (appendq! +ligatures-extra-symbols
            `(:checkbox      "☐"
              :pending       "◼"
              :checkedbox    "☑"
              :list_property "∷"
              :results       "🠶"
              :property      "☸"
              :properties    "⚙"
              :end           "∎"
              :options       "⌥"
              :title         "𝙏"
              :subtitle      "𝙩"
              :author        "𝘼"
              :date          "𝘿"
              :latex_header  "⇥"
              :latex_class   "🄲"
              :beamer_header "↠"
              :begin_quote   "❮"
              :end_quote     "❯"
              :begin_export  "⯮"
              :end_export    "⯬"
              :priority_a   ,(propertize "⚑" 'face 'all-the-icons-red)
              :priority_b   ,(propertize "⬆" 'face 'all-the-icons-orange)
              :priority_c   ,(propertize "■" 'face 'all-the-icons-yellow)
              :priority_d   ,(propertize "⬇" 'face 'all-the-icons-green)
              :priority_e   ,(propertize "❓" 'face 'all-the-icons-blue)
              :em_dash       "—"))
  (set-pretty-symbols! 'org-mode
    :merge t
    :name           "⁍"
    :checkbox      "[ ]"
    :pending       "[-]"
    :checkedbox    "[X]"
    :list_property "::"
    :results       "#+RESULTS:"
    :property      "#+PROPERTY:"
    :property      ":PROPERTIES:"
    :end           ":END:"
    :options       "#+OPTIONS:"
    :title         "#+TITLE:"
    :subtitle      "#+SUBTITLE:"
    :author        "#+AUTHOR:"
    :date          "#+DATE:"
    :latex_class   "#+LATEX_CLASS:"
    :latex_header  "#+LATEX_HEADER:"
    :beamer_header "#+BEAMER_HEADER:"
    :begin_quote   "#+BEGIN_QUOTE"
    :end_quote     "#+END_QUOTE"
    :begin_export  "#+BEGIN_EXPORT"
    :end_export    "#+END_EXPORT"
    :priority_a    "[#A]"
    :priority_b    "[#B]"
    :priority_c    "[#C]"
    :priority_d    "[#D]"
    :priority_e    "[#E]"
    :em_dash       "---")
  )

(after! org
  (org-superstar-mode)
  )

(setq org-ellipsis " ▼ ")

(setq org-hide-emphasis-markers t)      ; Hides *strong* /italic/ =highlight= marker

(after! org
  (setq org-priority-highest ?A
        org-priority-lowest ?E
        org-priority-faces
        '((?A . 'all-the-icons-red)
          (?B . 'all-the-icons-orange)
          (?C . 'all-the-icons-yellow)
          (?D . 'all-the-icons-green)
          (?E . 'all-the-icons-blue))))

(setq org-todo-keyword-faces '(
                               ("PROJ" . "DarkGreen")
                               ("EPIC" . (:foreground "DodgerBlue" :weight bold))
                               ("TODO" . org-warning)
                               ("IDEA" . (:foreground "BlueViolet"))
                               ("BACKLOG" . (:foreground "GreenYellow" :weight normal :slant italic :underline t))
                               ("PLAN" . (:foreground "Magenta1" :weight bold :underline t))
                               ("ACTIVE" . (:foreground "Systemyellowcolor" :weight bold :slant italic :underline t))
                               ("REVIEW" . (:foreground "Darkorange2" :weight bold :underline t))
                               ("WAIT" . (:foreground "yellow4" :weight light :slant italic))
                               ("HOLD" . (:foreground "red4"))
                               ("KILL" . "red")
                               ("CANCELLED" . (:foreground "red3" :weight bold :strike-through t))
                               )
      )

(defun jp/org-mode-setup ()
  (org-indent-mode 1)  ; Indent text following current headline
  (mixed-pitch-mode 1) ; Enable different Fonts
  ;;(org-roam-setup) ; Enable org-roam-db-autosync
  (setq org-image-actual-width nil) ; Set optional images
  (rainbow-mode 1)    ; Enable rainbow mode
  )
(add-hook 'org-mode-hook #'jp/org-mode-setup)

(defun jp/org-visual-fill-column ()
  (setq visual-fill-column-width 120  ; Margin width
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1) ; Enable Margins
  (visual-line-mode 1)  ; also show entire lines
  )

(add-hook 'org-mode-hook #'jp/org-visual-fill-column)

;; setting org headlines
(custom-set-faces!
   '(org-level-1 :inherit outline-1 :height 1.2)
   '(org-level-2 :inherit outline-2 :height 1.1)
   '(org-level-3 :inherit outline-3 :height 1.0)
   '(org-level-4 :inherit outline-4 :height 1.0)
   '(org-level-5 :inherit outline-5 :height 1.0)
  )

;; Make sure org-indent face is available
(require 'org-indent)

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block-begin-line nil :foreground "#999" :height 110 :inherit 'fixed-pitch)
(set-face-attribute 'org-block-end-line nil :foreground "#999" :height 110 :inherit 'fixed-pitch)
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
(set-face-attribute 'org-drawer nil :inherit 'fixed-pitch :foreground "SkyBlue4")

(setq org-roam-v2-ack t); Disable Warning for org-roam v2
(setq org-directory "~/org/"
      org-agenda-files '("~/org/Agenda.org"
                         "~/org/Tasks.org"
                         "~/org/Habits.org"
                         ;;"~/org/Emails.org"
                         )
      org-default-notes-file (concat org-directory "/Notes.org")
      org-clock-sound "~/sounds/ding.wav")

(setq auth-sources '((:source "~/.authinfo.gpg")))

(defun jp/org-roam-select-prefix (prefix)
  (org-roam-node-read
   nil
   (lambda (node)
     (string-prefix-p
      (concat org-roam-directory prefix)
      (org-roam-node-file node))
     )
   ))

(defun jp/org-roam-ignore-prefix (prefix)
  (org-roam-node-read
   nil
   (lambda (node)
     (not (string-prefix-p
           (concat org-roam-directory prefix)
           (org-roam-node-file node))
          ))
   ))

(defun jp/org-roam-ignore-literature ()
  (interactive)
  (jp/org-roam-ignore-prefix "/Literature"))

(defun jp/org-roam-select-literature ()
  (interactive)
  (jp/org-roam-select-prefix "/Literature"))

(defun jp/org-roam-ignore-pc ()
  (interactive)
  (jp/org-roam-ignore-prefix "/PC"))

(defun jp/org-roam-select-pc ()
  (interactive)
  (jp/org-roam-select-prefix "/PC"))

(defun jp/org-roam-ignore-projects ()
  (interactive)
  (jp/org-roam-ignore-prefix "/Projects"))

(defun jp/org-roam-select-projects ()
  (interactive)
  (jp/org-roam-select-prefix "/Projects"))

(defun jp/org-roam-ignore-other ()
  (interactive)
  (jp/org-roam-ignore-prefix "/20"))

(defun jp/org-roam-select-other ()
  (interactive)
  (jp/org-roam-select-prefix "/20"))

(defun jp/org-roam-get-tagged (&optional tag)
  (mapcar
   #'org-roam-node-file
   (seq-filter
    (lambda (node)
      (member tag (org-roam-node-tags node)))
    (org-roam-node-list))))

(defun jp/org-roam-agenda ()
  (interactive)
  (let ((org-agenda-files (jp/org-roam-get-tagged "Tasks")))
  (org-agenda)))

(setq org-templates-directory (concat doom-private-dir "/templates/"))
(defun jp/read-template (template)
  "Reading TEMPLATE as a file from org-templates-directory.
Returns file content as a string."
  (with-temp-buffer
    (insert-file-contents (concat org-templates-directory template))
    (buffer-string)))
(defun jp/read-newproject-template ()
  (jp/read-template "new-project.org"))
(defun jp/read-dailyreview-template ()
  (jp/read-template "daily-review.org"))
(defun jp/read-weekly-template ()
  (jp/read-template "weekly-review.org"))
(defun jp/read-monthly-template ()
  (jp/read-template "monthly-review.org"))
(defun jp/read-meeting-template ()
  (jp/read-template "Meeting.org"))
(defun jp/read-script-template ()
  (jp/read-template "script.org"))

(defun jp/daily-review ()
  (interactive)
  (let ((org-capture-templates '(("d" "Review: Daily Review" entry (file+olp+datetree "~/ZK/daily/reviews.org")
                                  (file "~/.doom.d/templates/daily-review.org")))))
    (progn
      (org-capture nil "d")
      (org-capture-finalize t)
      (org-speed-move-safe 'outline-up-heading)
      (org-narrow-to-subtree)
      (org-clock-in))))

(defun jp/weekly-review ()
  (interactive)
  (let ((org-capture-templates '(("d" "Review: Weekly Review" entry (file+olp+datetree "~/ZK/daily/reviews.org")
                                  (file "~/.doom.d/templates/weekly-review.org")))))
    (progn
      (org-capture nil "d")
      (org-capture-finalize t)
      (org-speed-move-safe 'outline-up-heading)
      (org-narrow-to-subtree)
      (org-clock-in))))

(defun jp/monthly-review ()
  (interactive)
  (let ((org-capture-templates '(("d" "Review: Monthly Review" entry (file+olp+datetree "~/ZK/daily/reviews.org")
                                  (file "~/.doom.d/templates/monthly-review.org")))))
    (progn
      (org-capture nil "d")
      (org-capture-finalize t)
      (org-speed-move-safe 'outline-up-heading)
      (org-narrow-to-subtree)
      (org-clock-in))))

(defun jp/go-to-projects (&optional name head)
  ""
  (interactive)
  (let* ((headline-regex (or head "* Projects"))
         (node (jp/org-roam-select-projects)))
    (org-roam-node-visit node)
    ;;(org-roam-node-find-noselect node)
    (widen)
    (beginning-of-buffer)
    (re-search-forward headline-regex)
    (beginning-of-line)))

(setq org-todo-keywords '(
                          (sequence "TODO(t)" "EPIC(e)" "PROJ(p)" "|"
                                "DONE(d)")
                          (sequence "BACKLOG(b)" "PLAN(P)" "ACTIVE(a)"
                                    "REVIEW(r)" "WAIT(W@/!)" "HOLD(h)" "|"
                                    "COMPLETED(c)" "KILL(k)" "CANCELLED(C)" "STOPPED(s@)")
                        )
      )

(setq org-capture-templates '(
                              ("a" "Agenda")
                              ("ah" "Home" entry (file+headline "~/org/Agenda.org" "Home")
                               "* TODO %?\n %i\n %a")
                              ("as" "Sys" entry (file+headline "~/org/Agenda.org" "Sys")
                               "* TODO %?\n %i\n %a")
                              ("f" "Fleeting Note" entry (file+headline "~/org/Notes.org" "Tasks")
                               "* %?\n %x\n %i\n %a")
                              ("M" "Meeting" entry
                               (file+olp+datetree "~/org/Meetings.org")
                               (function jp/read-meeting-template)
                               :clock-in :clock-resume
                               :empty-lines 1)
                              ("m" "Email Workflow")
                              ("mf" "Follow Up" entry (file+olp "~/org/Mail.org" "Follow Up")
                               "* TODO %a\n%?\n#+begin_quote\n%x\n#+end_quote")
                              ("mr" "Read Later" entry (file+olp "~/org/Mail.org" "Read Later")
                               "* TODO %a\n%?\n#+begin_quote\n%x\n#+end_quote%x")
                              ("l" "Logbook Entries")
                              ("ls" "Software" entry
                               (file+olp+datetree "~/org/Logbook.org")
                               "\n* %U %a%? :Software:"
                               :clock-in :clock-resume)
                              ("lh" "Hardware" entry
                               (file+olp+datetree "~/org/Logbook.org")
                               "\n* %U %a%? :Hardware:"
                               :clock-in :clock-resume)
                              ("lc" "Configuration" entry
                               (file+olp+datetree "~/org/Logbook.org")
                               "\n* %U %a%? :Configuration:"
                               :clock-in :clock-resume)
                              ("s" "Symptom Journal" entry (file+datetree "symptom-journal.org.gpg")
                               "* ~%<%H:%M>~ - %? :symptom:\n"
                               :time-prompt t
                               :unnarrowed t)
                              ("t" "Task Entries")
                              ("tt" "Todo Task" entry (file+headline "~/org/Notes.org" "Tasks")
                               "* TODO %?\n %i\n %a")
                              ("te" "Epic Task" entry (file+headline "~/org/Notes.org" "Epic")
                               "* EPIC %?\n %i\n %a")
                              ("ti" "New Idea" entry (file+headline "~/org/Notes.org" "Ideas")
                               "* IDEA %?\n %i\n %a")))

(setq org-roam-capture-templates
      '(("d" "default" plain
         "%?\n\nSee also %a.\n"
         :if-new (file+head
                  "%<%Y%m%d%H%M%S>-${slug}.org"
                  "#+title: ${title}\n")
         :unnarrowed t)
        ("j" "Projects" plain
         (function jp/read-newproject-template)
         :if-new (file+head
                  "Projects/%<%Y%m%d%H%M%S>-${slug}.org"
                  "#+title: ${title}\n")
         :clock-in :clock-resume
         :unnarrowed t
         )
        ("i" "Individuum / Persona" plain
         "%?\n\nSee also %a.\n"
         :if-new (file+head
                  "People/%<%Y%m%d%H%M%S>-${slug}.org"
                  "#+title: ${title}\n")
         :unnarrowed t
         )
        ("l" "Literature")
        ("ll" "Literature Note" plain
         "%?\n\nSee also %a.\n* Links\n- %x\n* Notes\n"
         :if-new (file+head
                  "Literature/%<%Y%m%d%H%M%S>-${slug}.org"
                  "#+title: ${title}\n")
         :unnarrowed t
         )
        ("lr" "Bibliography reference" plain
         "#+ROAM_KEY: %^{citekey}\n#+PROPERTY: type %^{entry-type}\n#+FILETAGS: %^{keywords}\n#+AUTHOR: %^{author}\n%?"
         :if-new (file+head
                  "References/${citekey}.org"
                  "#+title: ${title}\n")
         :unnarrowed t
         )
        ("p" "PC" plain
         "%?\n\nSee also %a.\n"
         :if-new (file+head
                  "PC/%<%Y%m%d%H%M%S>-${slug}.org"
                  "#+title: ${title}\n#+date: %U")
         :unnarrowed t
         )
        )
      )

(setq org-roam-capture-ref-templates '(
                                       ("r" "Reference" plain
                                        "%?\n\n* Citations\n#+begin_quote\n${body}\n#+end_quote"
                                        :if-new (file+head
                                                 "Literature/%<%Y%m%d%H%M%S>-${slug}.org"
                                                 "#+title: ${title}\n#+date: %U\n")
                                        :unnarrowed t
                                        )
                                       ("l" "Literature References" plain
                                        "%?\n\n* Abstract\n#+begin_quote\n${body}\n#+end_quote"
                                        :if-new (file+head
                                                 "References/%<%Y%m%d%H%M%S>-${slug}.org"
                                                 "#+title: ${title}\n#+date: %U\n#+ROAM_REF: ${ref}")
                                        :unnarrowed t
                                        :empty-lines 1)
                                       ("w" "Web site" entry
                                        :target (file+head
                                                 "Literature/%<%Y%m%d%H%M%S>-${slug}.org"
                                                 "#+title: ${title}\n#+date: %U\n")
                                        "* %a :website:\n\n%U %?\n\n#+begin_quote\n%:initial\n#+end_quote")
                                       )
      )

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :if-new (file+head
                  "%<%Y-%m-%d>.org"
                  "#+title: %<%Y-%m-%d>\n[[roam:%<%Y-%B>]]\n")
         :kill-buffer t
         )
        ("j" "Journal entry" entry
         "* ~%<%H:%M>~ - Journal  :journal:\n\n%?\n\n"
         :if-new (file+head+olp
                  "%<%Y-%m-%d>.org"
                  "#+title: %<%Y-%m-%d>\n"
                  ("Journal"))
         :kill-buffer t
         )
        ("l" "Monthly Log" entry
         "* %?\n  %U\n  %a\n  %i"
         :if-new (file+head+olp
                  "%<%Y-%B>.org"
                  "#+title: %<%Y-%B>\n"
                  ("Log"))
         :kill-buffer t
         )
        ("m" "meeting" entry
         (file "~/.dotfiles/doom/.doom.d/templates/Meeting.org")
         :if-new (file+head+olp
                  "%<%Y-%m-%d>.org"
                  "#+title: %<%Y-%m-%d>\n[[roam:%<%Y-%B>]]\n"
                  ("Meetings")))
        ("r" "Review")
        ("rd" "Daily Review" entry
         (file "~/.dotfiles/doom/.doom.d/templates/daily-review.org")
         :target (file+head
          "%<%Y-%m-%d>.org"
          "#+title: %<%Y-%m-%d>\n[[roam:%<%Y-%B>]]\n"))
        ("rm" "Monthly Review" entry
         (file "~/.dotfiles/doom/.doom.d/templates/monthly-review.org")
         :if-new (file+head
                  "%<%Y-%B>.org"
                  "#+title: %<%Y-%B>\n"))))

(setq org-agenda-custom-commands
      '(("d" "Dashboard"
         ((agenda "" ((org-deadline-warning-days 20)))
          (todo "BACKLOG"
                ((org-agenda-overriding-header "Backlog Tasks")))
          (todo "ACTIVE" ((org-agenda-overriding-header "Active Tasks")))
          (todo "REVIEW" ((org-agenda-overriding-header "Active Reviews")))
          (todo "EPIC" ((org-agenda-overriding-header "Active Epics")))))

        ("T" "All Todo Tasks"
         ((todo "TODO"
                ((org-agenda-overriding-header "Todo Tasks")))))

        ("W" "Work Tasks" tags-todo "+work")

        ;; Low-effort next actions
        ("E" tags-todo "+TODO=\"EPIC\"+Effort<15&+Effort>0"
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
                 (org-agenda-files org-agenda-files)))))
        ("h" "Daily habits"
         ((agenda ""))
         ((org-agenda-show-log t)
          (org-agenda-ndays 14)
          (org-agenda-log-mode-items '(state))
          (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":DAILY:"))))
        ;; other commands here
        ))

(setq org-tag-alist
      '((:startgroup)
         ; Put mutually exclusive tags here
         (:endgroup)
         ("@sys" . ?S)
         ("@home" . ?H)
         ("@work" . ?W)
         ("planning" . ?p)
         ("publish" . ?P)
         ("batch" . ?b)
         ("note" . ?n)
         ("idea" . ?i)))

(setq org-lowest-priority ?E) ;; Priorities A to E

(setq org-refile-targets
      '(("Archive.org" :maxlevel . 1)
        ("Tasks.org" :maxlevel . 1)))

(setq org-archive-location ".archive/%s::")

;; Save Org buffers after refiling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)

(add-to-list 'org-modules 'org-habit)

;; (setq org-latex-to-pdf-process '("texi2dvi --pdf --clean --verbose --batch %f"))

(require 'ox-latex)
(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))

;; Define Koma Article Class
(add-to-list 'org-latex-classes
             '("koma-article"
               "\\documentclass{scrartcl}"
               ("\\section{%s}" . "\\section*{%s}")))

;; Define Review of Scientific Instruments Class
(add-to-list 'org-latex-classes
             '("aip-rsi"
               "\\documentclass[
                aip, % AIP Journals
                rsi, % Review of Scientific Instruments
                amsmath,amssymb, % Basic Math Packages
                preprint, % or reprint
                ]{revtex4-2}
\\include{structure}
[NO-DEFAULT-PACKAGES]
[NO-EXTRA]
[NO-PACKAGES]

%% Apr 2021: AIP requests that the corresponding
%% email to be moved after the affiliations
\\makeatletter
\\def\\@email#1#2{%
 \\endgroup
 \\patchcmd{\\titleblock@produce}
  {\\frontmatter@RRAPformat}
  {\\frontmatter@RRAPformat{\\produce@RRAP{*#1\\href{mailto:#2}{#2}}}\\frontmatter@RRAPformat}
  {}{}
}%
\\makeatother"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ))

(defun org-export-latex-no-toc (depth)
  (when depth
    (format "%% Org-mode is exporting headings to %s levels.\n"
            depth)))
(setq org-export-latex-format-toc-function 'org-export-latex-no-toc)

(add-to-list 'org-latex-classes
      '("letter"
         "\\documentclass[
    fontsize=12pt,
    % Satzspiegel
    DIV=13,
    paper=a4,
    enlargefirstpage=on,
    pagenumber=headright,
    %---------------------------------------------------------------------------
    % Layout
    headsepline=on,
    parskip=half,
    %---------------------------------------------------------------------------
    % Briefkopf und Anschrift
    %fromalign=location,
    fromphone=off,
    fromrule=off,
    fromfax=off,
    fromemail=on,
    fromurl=on,
    fromlogo=off,
    addrfield=on,
    backaddress=off,
    subject=beforeopening,
    locfield=narrow,
    foldmarks=on,
    numericaldate=off,
    refline=narrow,
    draft=off
          ]{scrlttr2}
\\include{structure}
[NO-DEFAULT-PACKAGES]
[NO-EXTRA]
[NO-PACKAGES]
\\usepackage[T1]{fontenc}
\\usepackage[utf8]{inputenc}
\\usepackage{url}
\\usepackage{graphicx}
\\usepackage{uniinput}
% Fonts
\\setkomafont{fromname}{\\sffamily}
\\setkomafont{fromaddress}{\\sffamily}
\\setkomafont{pagenumber}{\\sffamily}
\\setkomafont{subject}{\\mdseries \\bfseries}
\\setkomafont{backaddress}{\\mdseries}
\\usepackage{mathptmx}%% Schrift Times
"
         ("\\textbf{%s}" . "\\textbf*{%s}")
         ("\\textbf{%s}" . "\\textbf*{%s}")
         ))

(add-to-list 'org-link-abbrev-alist '("ody5" . "https://gitlab.ody5.de/"))
(add-to-list 'org-link-abbrev-alist '("gitlab" . "https://gitlab.com/"))

(setq plantuml-default-exec-mode 'jar)

(require 'org-alert)

(setq +org-msg-accent-color "#1a5fb4"
      org-msg-greeting-fmt "\nHi %s,\n\n"
      org-msg-signature "\n\n#+begin_signature\nAll the best,\\\\\n@@html:<b>@@Jonathan@@html:</b>@@\n#+end_signature")
(map! :map org-msg-edit-mode-map
      :after org-msg
      :n "G" #'org-msg-goto-body)

(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
  (add-to-list 'org-structure-template-alist '("uml" . "src plantuml :file uml.png"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
  (add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("go" . "src go"))
  (add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
  (add-to-list 'org-structure-template-alist '("json" . "src json")))

;; Enable Special Blocks in Org-Mode
(add-hook #'org-mode-hook #'org-special-block-extras-mode)

;; Use short names like ‘defblock’ instead of the fully qualified name
;; ‘org-special-block-extras--defblock’
;; (org-special-block-extras-short-names)

(defun jp/presentation-setup()
  ;;(setq text-scale-mode-amount 3)
  ;;(text-scale-mode 1)
  (org-display-inline-images)
  (org-tree-slide-activate-message "Presentation started!")
  (org-tree-slide-deactivate-message "Presentation finished!")
  (org-tree-slide-header t)
  (org-tree-slide-breadcrumbs " // ")
  )

(defun jp/presentation-end()
  ;;(text-scale-mode 0)
  )

(add-hook #'org-tree-slide-play #'jp/presentation-setup)
(add-hook #'org-tree-slide-stop #'jp/presentation-end)

;; Enable PlantUML Diagrams
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
;; Jar Configuration
(setq org-plantuml-jar-path (concat (getenv "HOME") "/.emacs.d/.local/etc/plantuml.jar"))
(setq plantuml-jar-path (concat (getenv "HOME") "/.emacs.d/.local/etc/plantuml.jar"))
(setq plantuml-default-exec-mode 'jar)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (python . t)
   (LaTeX . t)
   (plantuml . t)
   (emacs-lisp . t)))

(setq org-babel-tangle-comment-format-beg ""
      org-babel-tangle-comment-format-end "")

(use-package! org-roam-bibtex
  :after org-roam
  :config
  (require 'org-ref)) ; optional: if Org Ref is not loaded anywhere else, load it here

;; Helm Autocompletion
(autoload 'helm-bibtex "helm-bibtex" "" t)

;; Ivy Autocompletion
;;(autoload 'ivy-bibtex "ivy-bibtex" "" t)
;; ivy-bibtex requires ivy's `ivy--regex-ignore-order` regex builder, which
;; ignores the order of regexp tokens when searching for matching candidates.
;; Add something like this to your init file:
;; (setq ivy-re-builders-alist
;;       '((ivy-bibtex . ivy--regex-ignore-order)
;;         (t . ivy--regex-plus)))

(setq bibtex-file-path (concat org-roam-directory "/BibTeX/")
      bibtex-completion-bibliography '("~/ZK/BibTeX/Library.bib"
                                       "~/ZK/BibTeX/Master.bib"
                                       "~/Projects/Method-Paper/bibliography.bib")
      bibtex-completion-library-path '("~/nc/Library/BibTeX/")
      bibtex-completion-notes-path "~/ZK/References/")

(require 'org-protocol)    ; Enable org protocol for links (org-roam://...)
(require 'org-roam-protocol)
(require 'org-protocol-capture-html)

(setq org-roam-directory (file-truename "~/ZK")   ; Set org-roam directory
      org-roam-dailies-directory (file-truename "~/ZK/daily")
      org-attach-id-dir (concat org-roam-directory "/.attachments")
      org-id-locations-file "~/ZK/.orgids"
      org-roam-completion-everywhere nil
      org-roam-completion-system 'default
      org-roam-db-location "~/.emacs.d/org-roam.db"
      ;;org-roam-graph-executable "neato" ; or "dot" (default)
      )

(setq org-roam-mode-section-functions
      (list #'org-roam-backlinks-section
            #'org-roam-reflinks-section
            #'org-roam-unlinked-references-section
            ))

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;    :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(after! org
  (custom-set-faces!
    '(org-document-title :height 1.2)))

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
      ;; (:prefix ("d" . "dired")
      ;;  :desc "Open dired" "d" #'dired
      ;;  :desc "Dired jump to current" "j" #'dired-jump)
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
  (lsp-deferred))

(add-hook 'python-mode-hook #'jp/python-mode-hook)

;; NOTE: Set these if Python 3 is called "python3" on your system!
(setq dap-python-debugger 'debugpy)

(setq python-shell-interpreter "/opt/homebrew/Caskroom/miniforge/base/envs/labbook/bin/python")
(setq dap-python-executable "/opt/homebrew/Caskroom/miniforge/base/envs/labbook/bin/python")
(setq lsp-python-ms-python-executable-cmd "/opt/homebrew/Caskroom/miniforge/base/envs/labbook/bin/python")

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

;; Optional Magit Configuration

;; Tell Emacs where to find mu4e (only necessary if manual compiled)
(pcase system-type
  ((or 'gnu/linux 'windows-nt 'cygwin)
   (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e"))
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

  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "All Inboxes"
                :query "maildir:/Mailbox/INBOX OR maildir:/Personal/Inbox"
                :key ?i))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "Uni-Frankfurt"
                :query "from:/.*@uni-frankfurt/ OR maildir:/Personal/Uni"
                :key ?g))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "Family"
                :query "from:baerbel OR from:pieper OR from:kaiser OR from:kessler OR from:thewake35 OR maildir:/Mailbox/familie"
                :key ?m))

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)

  (setq jp/mu4e-inbox-query
        "(maildir:/Personal/Inbox OR maildir:/Mailbox/INBOX) AND flag:unread")

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
