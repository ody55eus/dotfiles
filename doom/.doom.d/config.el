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

(server-start)  ; Start Emacs as Server!

(map! :leader
      (:prefix ("b" . "buffer")
       :desc "Counsel buffer" :n "j" #'counsel-switch-buffer
       :desc "Counsel buffer other window" :n "J" #'counsel-switch-buffer-other-window
       :desc "List bookmarks" "L" #'list-bookmarks
       :desc "Save current bookmarks to bookmark file" "w" #'bookmark-save)
      ;; (:prefix-map ("c" . "code"))
      ;; (:prefix-map ("d" . "dired"))
      ;; (:prefix-map ("f" . "file"))
      ;; (:prefix-map ("g" . "git"))
      ;; (:prefix-map ("h" . "help"))
      ;; (:prefix-map ("i" . "emoji"))
      ;; (:prefix-map ("m" . "org manage")
      ;;  (:prefix ("a" . "attatch"))
      ;;  (:prefix ("b" . "table"))
      ;;  (:prefix ("c" . "clock"))
      ;;  (:prefix ("d" . "date"))
      ;;  )
      ;; (:prefix-map ("o" . "open"))
      ;; (:prefix-map ("p" . "projectile"))
      ;; (:prefix-map ("q" . "quit"))
      ;; (:prefix-map ("s" . "search"))
      (:prefix ("t" . "toogle")
       :desc "Toggle Cmd Log Buffer" "b" #'clm/toggle-command-log-buffer
       :desc "Toggle Global Cmd Log" "c" #'global-command-log-mode
       :desc "Toggle line highlight in frame" "h" #'hl-line-mode
       :desc "Toggle line highlight globally" "H" #'global-hl-line-mode
       :desc "Toggle truncate lines" "t" #'toggle-truncate-lines
       )
      (:prefix ("w" . "window")
       :desc "evil-window-left" :n "<left>" #'evil-window-left
       :desc "evil-window-right" :n "<right>" #'evil-window-right
       :desc "evil-window-up" :n "<up>" #'evil-window-up
       :desc "evil-window-down" :n "<down>" #'evil-window-down
       )
      (:prefix-map ("n" . "notes")
       (:prefix ("r" . "roam")
        :desc "Complete org-roam " :n "c" #'org-roam-complete-at-point
        :desc "New Daily Node (today)" :n "t" #'org-roam-dailies-capture-today
        :desc "Find org-roam Node" :n "f" #'org-roam-node-find
        :desc "Insert org-roam Node" :n "i" #'org-roam-node-insert
        :desc "Toggle org-roam Buffer" :n "l" #'org-roam-buffer-toggle
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

;; (map! :leader
;;       (:prefix-map ("l" . "lookup")
;;        )
;;       )

(map! (:prefix-map ("C-w" . "window")
       :desc "evil-window-left" :n "<left>" #'evil-window-left
       :desc "evil-window-right" :n "<right>" #'evil-window-right
       :desc "evil-window-up" :n "<up>" #'evil-window-up
       :desc "evil-window-down" :n "<down>" #'evil-window-down
       )
 )

(setq doom-theme 'doom-dark+)

;; Set the font face based on platform
(pcase system-type
  ((or 'gnu/linux 'windows-nt 'cygwin)
   (set-face-attribute 'default nil
                       :font "Source Code Pro"
                       :weight 'regular
                       :height 140))
  ('darwin (set-face-attribute 'default nil :font "Liberation Mono for Powerline" :height 140)))

;; Set the fixed pitch face
(pcase system-type
  ((or 'gnu/linux 'windows-nt 'cygwin)
   (set-face-attribute 'fixed-pitch nil
                       :font "Source Code Pro"
                       :weight 'regular
                       :height 140))
  ('darwin (set-face-attribute 'fixed-pitch nil :font "Liberation Mono for Powerline" :height 140)))

;; Set the variable pitch face
(pcase system-type
  ((or 'gnu/linux 'windows-nt 'cygwin)
   (set-face-attribute 'variable-pitch nil
                       ;; :font "Cantarell"
                       :font "Roboto"
                       :height 185
                       :weight 'light))
  ('darwin (set-face-attribute 'variable-pitch nil :font "Hiragino Sans" :height 150)))

(setq display-line-numbers-type 'relative)

;; Disable Line Numbers for specific modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Set up the visible bell
(setq visible-bell t)

(menu-bar-mode 1)

(defun jp/set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
  (progn
    ;; use 120 char wide window for largeish displays
    ;; and smaller 80 column windows for smaller displays
    ;; pick whatever numbers make sense for you
    (if (> (x-display-pixel-width) 1280)
           (add-to-list 'default-frame-alist (cons 'width 177))
           (add-to-list 'default-frame-alist (cons 'width 100)))
    ;; for the height, subtract a couple hundred pixels
    ;; from the screen height (for panels, menubars and
    ;; whatnot), then divide by the height of a char to
    ;; get the height we want
    (add-to-list 'default-frame-alist
         (cons 'height (/ (- (x-display-pixel-height) 120)
                             (frame-char-height)))))))

(jp/set-frame-size-according-to-resolution)

(add-hook 'org-mode-hook (rainbow-mode))

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

(setq org-roam-v2-ack t); Disable Warning for org-roam v2
(setq org-directory "~/org/"
      org-agenda-files '("~/org/Agenda.org"
                         "~/org/Tasks.org"
                         "~/org/Habits.org"
                         "~/org/Emails.org")
      org-default-notes-file (concat org-directory "/Notes.org")
      org-clock-sound "~/sounds/ding.wav")

(setq auth-sources '((:source "~/.authinfo.gpg")))

(require 'org-protocol)    ; Enable org protocol for links (org-roam://...)
(require 'org-roam-protocol)
(require 'org-protocol-capture-html)

(setq org-roam-directory (file-truename "~/ZK")   ; Set org-roam directory
      org-roam-dailies-directory (file-truename "~/ZK/daily")
      org-attach-id-dir (concat org-roam-directory "/Literature/.attach")
      org-roam-completion-everywhere t
      org-roam-completion-system 'default)

(setq org-ellipsis " ▼ ")

(defun jp/org-mode-setup ()
  (org-indent-mode)
  (mixed-pitch-mode 1) ; Enable different Fonts
  (org-roam-setup) ; Enable org-roam
  (setq org-image-actual-width nil) ; Set optional images
  (visual-line-mode 1))

(add-hook 'org-mode-hook #'jp/org-mode-setup)

(setq org-hide-emphasis-markers t)      ; Hides *strong* /italic/ =highlight= marker

(defun jp/org-visual-fill-column ()
  (setq visual-fill-column-width 120
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

(setq org-todo-keyword-faces '(
                               ("PROJ" . "DarkGreen")
                               ("EPIC" . (:foreground "DodgerBlue" :weight bold))
                               ("TODO" . org-warning)
                               ("IDEA" . (:foreground "BlueViolet"))
                               ("BACKLOG" . (:foreground "GreenYellow" :weight normal :slant italic :underline t))
                               ("PLAN" . (:foreground "DarkMagenta" :weight bold :underline t))
                               ("ACTIVE" . (:foreground "" :weight bold :underline t))
                               ("REVIEW" . (:foreground "" :weight bold :underline t))
                               ("WAIT" . (:foreground "yellow4" :weight light :slant italic))
                               ("HOLD" . (:foreground "red4"))
                               ("KILL" . "red")
                               ("CANCELLED" . (:foreground "red3" :weight bold :strike-through t))
                               )
      )

(setq org-todo-keywords '(
                          (sequence "TODO(t)" "EPIC(e)" "PROJ(p)" "|"
                                "DONE(d)")
                          (sequence "BACKLOG(b)" "PLAN(P)" "ACTIVE(a)"
                                    "REVIEW(r)" "WAIT(W@/!)" "HOLD(h)" "|"
                                    "COMPLETED(c)" "KILL(k)" "CANCELLED(C)" "STOPPED(s@)")
                        )
      )

(setq org-capture-templates '(("f" "Fleeting Note" entry (file+headline "~/org/Notes.org" "Tasks")
                               "* %?\n %x\n %i\n %a")))

(add-to-list 'org-capture-templates '(("a" "Agenda")
                                     ("ah" "Home" entry (file+headline "~/org/Agenda.org" "Home")
                                      "* TODO %?\n %i\n %a")
                                     ("as" "Sys" entry (file+headline "~/org/Agenda.org" "Sys")
                                      "* TODO %?\n %i\n %a")) t)

(add-to-list 'org-capture-templates '(("m" "Email Workflow")
                              ("mf" "Follow Up" entry (file+olp "~/org/Mail.org" "Follow Up")
                               "* TODO %a")
                              ("mr" "Read Later" entry (file+olp "~/org/Mail.org" "Read Later")
                               "* TODO %a")) t)

(add-to-list 'org-capture-templates '(("t" "Task Entries")
                              ("tt" "Todo Task" entry (file+headline "~/org/Tasks.org" "Tasks")
                               "* TODO %?\n %i\n %a")
                              ("te" "Epic Task" entry (file+headline "~/org/Tasks.org" "Epic")
                               "* EPIC %?\n %i\n %a")
                              ("ti" "New Idea" entry (file+headline "~/org/Tasks.org" "Ideas")
                               "* IDEA %?\n %i\n %a")) t)

(add-to-list 'org-capture-templates '(("s" "Create Org Scripts")
                              ("ss" "shell" file
                               (file+headline "~/org/scripts/${name}.org")
                               "\n* Shell Script:\n\n#+begin_src sh :tangle ./${name}.sh\n\n%?\n\n#+end_src"
                               :clock-in :clock-resume
                               :empty-lines 1)) t)

(add-to-list 'org-capture-templates '(("l" "Logbook Entries")
                                      ("ls" "Software" entry
                                       (file+olp+datetree "~/org/Logbook.org")
                                       "\n* %<%I:%M %p> - Software :Software:\n\n%?\n\n"
                                       ;; ,(jp/read-file-as-string "~/Notes/Templates/Daily.org")
                                       :clock-in :clock-resume
                                       :empty-lines 1)
                                      ("lh" "Hardware" entry
                                       (file+olp+datetree "~/org/Logbook.org")
                                       "\n* %<%I:%M %p> - Hardware :Hardware:\n\n%?\n\n"
                                       :clock-in :clock-resume
                                       :empty-lines 1)
                                      ("lc" "Configuration" entry
                                       (file+olp+datetree "~/org/Logbook.org")
                                       "\n* %<%I:%M %p> - Configuration :Configuration:\n\n%?\n\n"
                                       :clock-in :clock-resume
                                       :empty-lines 1)) t)

(add-to-list 'org-capture-templates '(("M" "Meeting" entry
                                       (file+olp+datetree "~/org/Meetings.org")
                                       "* %<%H:%M> - %a :meetings:\n\n%?\n\n"
                                       :clock-in :clock-resume
                                       :empty-lines 1)) t)

(add-to-list 'org-capture-templates '(("w" "Web site" entry
                                       (file "")
                                       "* %a :website:\n\n%U %?\n\n%:initial")
                                      :empty-lines 1) t)

(add-to-list 'org-capture-templates '(("r" "References" plain
                                       "%?\n\n* Citations\n%x"
                                       :if-new (file+head
                                                "Literature/%<%Y%m%d%H%M%S>-${slug}.org"
                                                "#+title: ${title}\n#+date: %U\n#+ROAM_REF: ${ref}")
                                       :unnarrowed t)
                                      :empty-lines 1) t)

(setq org-roam-capture-templates
      '(("d" "default" plain
         "%?\n\nSee also %a.\n"
         :if-new (file+head
                  "%<%Y%m%d%H%M%S>-${slug}.org"
                  "#+title: ${title}\n")
         :unnarrowed t)
        ("c" "Coding" plain
         "%?\n\nSee also %a.\n"
         :if-new (file+head
                  "Coding/%<%Y%m%d%H%M%S>-${slug}.org"
                  "#+title: ${title}\n")
         :clock-in :clock-resume
         :unnarrowed t
         )
        ("e" "Person" plain
         "%?\n\nSee also %a.\n"
         :if-new (file+head
                  "People/%<%Y%m%d%H%M%S>-${slug}.org"
                  "#+title: ${title}\n")
         :clock-in :clock-resume
         :unnarrowed t
         )
        ("l" "Literature" plain
         "%?\n\nSee also %a.\n* Links\n- %x\n* Notes\n"
         :if-new (file+head
                  "Literature/%<%Y%m%d%H%M%S>-${slug}.org"
                  "#+title: ${title}\n")
         :clock-in :clock-resume
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
                                        "%?\n\n* Citations\n%x"
                                        :if-new (file+head
                                                 "Literature/%<%Y%m%d%H%M%S>-${slug}.org"
                                                 "#+title: ${title}\n#+date: %U\n")
                                        :unnarrowed t
                                        )
                                       )
      )

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :if-new (file+head
                  "%<%Y-%m-%d>.org"
                  "#+title: %<%Y-%m-%d>\n\n[[roam:%<%Y-%B>]]\n\n"
                  :kill-buffer t)
         )
        ("j" "Journal entry" entry
         "* ~%<%H:%M>~ - Journal  :journal:\n\n%?\n\n"
         :if-new (file+head+olp
                  "%<%Y-%m-%d>.org"
                  "#+title: %<%Y-%m-%d>\n\n"
                  ("Journal")
                  :kill-buffer t)
         )
        ("l" "Log" entry
         "* %?\n  %U\n  %a\n  %i"
         :if-new (file+head+olp
                  "%<%Y-%B>.org"
                  "#+title: %<%Y-%m-%d>\n\n[[roam:%<%Y-%B>]]\n\n"
                  ("Log")
                  :kill-buffer t)
         )
        ("m" "meeting" entry
         "* ~%<%H:%M>~ - %^{Meeting Title} :meetings:\n\n%?\n\n"
         :if-new (file+head+olp
                  "%<%Y-%m-%d>.org"
                  "#+title: %<%Y-%m-%d>\n\n[[roam:%<%Y-%B>]]\n\n"
                  ("Meetings")))))

(setq org-agenda-custom-commands
      '(("d" "Dashboard"
         ((agenda "" ((org-deadline-warning-days 20)))
          (todo "BACKLOG"
                ((org-agenda-overriding-header "Backlog Tasks")))
          (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))
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
         ("agenda" . ?a)
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
                ]{revtex4-1}
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

(require 'org-alert)

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
(setq plantuml-default-exec-mode 'jar)

;; Sample executable configuration
;;(setq plantuml-executable-path "/path/to/your/copy/of/plantuml.bin")
;;(setq plantuml-default-exec-mode 'executable)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (python . t)
   (LaTeX . t)
   (plantuml . t)
   (emacs-lisp . t)))

(use-package! org-roam-bibtex
  :after org-roam
  :config
  (require 'org-ref)) ; optional: if Org Ref is not loaded anywhere else, load it here

;; Helm Autocompletion
;;(autoload 'helm-bibtex "helm-bibtex" "" t)

;; Ivy Autocompletion
(autoload 'ivy-bibtex "ivy-bibtex" "" t)
;; ivy-bibtex requires ivy's `ivy--regex-ignore-order` regex builder, which
;; ignores the order of regexp tokens when searching for matching candidates.
;; Add something like this to your init file:
(setq ivy-re-builders-alist
      '((ivy-bibtex . ivy--regex-ignore-order)
        (t . ivy--regex-plus)))

(setq bibtex-completion-bibliography '((concat (getenv "HOME") "/ZK/Literature/Library.bib")))
(setq bibtex-completion-library-path '((concat (getenv "HOME") "/ZK/Literature/.attach")))

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

(defun jp/python-mode-hook()
  (require 'lsp-pyright)
  (require 'dap-python)
  (lsp-deferred))

(add-hook 'python-mode-hook #'jp/python-mode-hook)

;; NOTE: Set these if Python 3 is called "python3" on your system!
(setq python-shell-interpreter "python3")
(setq dap-python-executable "python3")
(setq dap-python-debugger 'debugpy)

(use-package pyvenv
  :after python-mode
  :config
  (pyvenv-mode 1))

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
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

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
