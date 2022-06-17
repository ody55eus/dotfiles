  ;; -*- lexical-binding: t; -*-

  (setq org-directory
          (file-truename "~/ZK"))

  ;; (setq org-agenda-files `(,org-directory))
  (defun jp/org-path (path)
    (expand-file-name path org-directory))

  (setq org-default-notes-file (jp/org-path "Notes.org"))

  (with-eval-after-load 'org-roam
    (defun jp/org-roam-project-finalize-hook ()
      "Adds the captured project file to `org-agenda-files' if the
  capture was not aborted."
      ;; Remove the hook since it was added temporarily
      (remove-hook 'org-capture-after-finalize-hook #'jp/org-roam-project-finalize-hook)

      ;; Add project file to the agenda list if the capture was confirmed
      (unless org-note-abort
        (with-current-buffer (org-capture-get :buffer)
          (add-to-list 'org-agenda-files (buffer-file-name)))))

    (defun jp/org-roam-find-project ()
      (interactive)
      ;; Add the project file to the agenda after capture is finished
      (add-hook 'org-capture-after-finalize-hook #'jp/org-roam-project-finalize-hook)

      ;; Select a project file to open, creating it if necessary
      (org-roam-node-find
       nil
       nil
       (jp/org-roam-filter-by-tag "Project")
       :templates
       '(("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
          :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
          :unnarrowed t))))

    (defun jp/org-roam-capture-inbox ()
      (interactive)
      (org-roam-capture- :node (org-roam-node-create)
                         :templates '(("i" "inbox" plain "* %?"
                                       :if-new (file+head "Inbox.org" "#+title: Inbox\n")))))

    (defun jp/org-roam-copy-todo-to-today ()
      (interactive)
      (let ((org-refile-keep t) ;; Set this to nil to delete the original!
            (org-roam-dailies-capture-templates
             '(("t" "tasks" entry "%?"
                :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
            (org-after-refile-insert-hook #'save-buffer)
            today-file
            pos)
        (save-window-excursion
          (org-roam-dailies--capture (current-time) t)
          (setq today-file (buffer-file-name))
          (setq pos (point)))

        ;; Only refile if the target file is different than the current file
        (unless (equal (file-truename today-file)
                       (file-truename (buffer-file-name)))
          (org-refile nil nil (list "Tasks" today-file nil pos)))))

    ;; (add-to-list 'org-after-todo-state-change-hook
    ;;              (lambda ()
    ;;                (when (equal org-state "DONE")
    ;;                  (jp/org-roam-copy-todo-to-today))))
    )

(setq org-roam-v2-ack t  ; Disable Warning for org-roam v2
      org-default-notes-file (jp/org-path "Notes.org"))

(setq auth-sources '((:source "~/.authinfo.gpg")))

(defhydra jp/org-roam-jump-menu (:hint nil)
  "
  ^Dailies^        ^Capture^       ^Jump^
  ^^^^^^^^-------------------------------------------------
  _t_: today       _T_: today       _m_: current month
  _r_: tomorrow    _R_: tomorrow    _e_: current year
  _y_: yesterday   _Y_: yesterday   ^ ^
  _d_: date        ^ ^              ^ ^
  "
  ("t" org-roam-dailies-goto-today)
  ("r" org-roam-dailies-goto-tomorrow)
  ("y" org-roam-dailies-goto-yesterday)
  ("d" org-roam-dailies-goto-date)
  ("T" org-roam-dailies-capture-today)
  ("R" org-roam-dailies-capture-tomorrow)
  ("Y" org-roam-dailies-capture-yesterday)
  ("m" jp/org-roam-goto-month)
  ("e" jp/org-roam-goto-year)
  ("c" nil "cancel"))

(after! org
  (setq org-modern-todo nil      ; Don't update TODO Tags
      org-modern-block nil     ; #+BEGIN block/src/example etc.
      org-modern-keyword nil   ; #+AUTHOR / #+TITLE / #+PROPERTIES etc.
      org-modern-priority nil  ; Don't update task priorities
      org-modern-star ["◉" "○" "✸" "✿"]  ; use pretty stars
      )
  )

(after! org
  (appendq! +ligatures-extra-symbols
            `(:checkbox      "☐"
              :pending       "◼"
              :checkedbox    "☑"
              :list_property "∷"
              :results       "🠶"
              :property      ""
              :properties    ""
              :end           ""
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
              ;; :em_dash       "—"
              ))
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
    ;; :em_dash       "---"
    )
  (setq org-ellipsis " ▼ ")
  (setq org-hide-emphasis-markers t)      ; Hides *strong* /italic/ =highlight= marker
  )

(after! org
  (setq org-priority-highest ?A
        org-priority-lowest ?E
        org-priority-faces
        '((?A . 'all-the-icons-red)
          (?B . 'all-the-icons-orange)
          (?C . 'all-the-icons-yellow)
          (?D . 'all-the-icons-green)
          (?E . 'all-the-icons-blue))))

(defun jp/org-visual-fill-column ()
  (setq visual-fill-column-width 120  ; Margin width
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1) ; Enable Margins
  (visual-line-mode 1)  ; also show entire lines
  )

(add-hook 'org-mode-hook #'jp/org-visual-fill-column)

(defun jp/org-mode-setup ()
  (org-indent-mode 1)  ; Indent text following current headline
  (mixed-pitch-mode 1) ; Enable different Fonts
  ;;(org-roam-setup) ; Enable org-roam-db-autosync
  (setq org-image-actual-width nil) ; Set optional images
  (rainbow-mode 1)    ; Enable rainbow mode
  (emojify-mode 1)    ; Enable Emojis
  (org-appear-mode 1) ; re-appear markup signs =*~
  
    (org-modern-mode 1)
  )
(add-hook 'org-mode-hook #'jp/org-mode-setup)

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
;;(set-face-attribute 'org-block-begin-line nil :foreground "#999" :height 80 :inherit 'fixed-pitch)
;;(set-face-attribute 'org-block-end-line nil :foreground "#999" :height 80 :inherit 'fixed-pitch)
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
      (and
     (member tag-name (org-roam-node-tags node))
     (eq (org-roam-node-level node) 0)))
    (org-roam-node-list))))

(defun jp/org-roam-filter-by-tag (tag-name)
  (lambda (node)
    (and
     (member tag-name (org-roam-node-tags node))
     (eq (org-roam-node-level node) 0))))

(defun jp/org-roam-list-notes-by-tag (tag-name)
  (mapcar #'org-roam-node-file
          (seq-filter
           (jp/org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))

(defun jp/org-roam-agenda ()
  (interactive)
  (let ((org-agenda-files (jp/org-roam-list-notes-by-tag "Project")))
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
  (let ((org-capture-templates '(("d" "Review: Daily Review" entry (file+olp+datetree "~/share/notes/daily/reviews.org")
                                  (file "~/.doom.d/templates/daily-review.org")))))
    (progn
      (org-capture nil "d")
      (org-capture-finalize t)
      (org-speed-move-safe 'outline-up-heading)
      (org-narrow-to-subtree)
      (org-clock-in))))

(defun jp/weekly-review ()
  (interactive)
  (let ((org-capture-templates '(("d" "Review: Weekly Review" entry (file+olp+datetree "~/share/notes/daily/reviews.org")
                                  (file "~/.doom.d/templates/weekly-review.org")))))
    (progn
      (org-capture nil "d")
      (org-capture-finalize t)
      (org-speed-move-safe 'outline-up-heading)
      (org-narrow-to-subtree)
      (org-clock-in))))

(defun jp/monthly-review ()
  (interactive)
  (let ((org-capture-templates '(("d" "Review: Monthly Review" entry (file+olp+datetree "~/share/notes/daily/reviews.org")
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

(defun jp/org-roam-goto-month ()
  (interactive)
  (org-roam-capture- :goto (when (org-roam-node-from-title-or-alias (format-time-string "%Y-%B")) '(4))
                     :node (org-roam-node-create)
                     :templates '(("m" "month" plain "\n* Goals\n\n%?* Summary\n\n"
                                   :if-new (file+head "%<%Y-%B>.org"
                                                      "#+title: %<%Y-%B>\n#+filetags: Project\n")
                                   :unnarrowed t))))

(defun jp/org-roam-goto-year ()
  (interactive)
  (org-roam-capture- :goto (when (org-roam-node-from-title-or-alias (format-time-string "%Y")) '(4))
                     :node (org-roam-node-create)
                     :templates '(("y" "year" plain "\n* Goals\n\n%?* Summary\n\n"
                                   :if-new (file+head "%<%Y>.org"
                                                      "#+title: %<%Y>\n#+filetags: Project\n")
                                   :unnarrowed t))))

(setq calendar-holidays
  (append holiday-general-holidays holiday-local-holidays
          holiday-other-holidays holiday-christian-holidays
          holiday-solar-holidays))

(defun jp/org-roam-refresh-agenda-list ()
  (interactive)
  (setq org-agenda-files (jp/org-roam-list-notes-by-tag "Project"))
  (dolist (node (jp/org-roam-list-notes-by-tag "Tasks"))
    (add-to-list 'org-agenda-files node))
  (add-to-list 'org-agenda-files (jp/org-path "Agenda.org"))
  (add-to-list 'org-agenda-files (jp/org-path "Habits.org")))

(add-hook! 'org-roam-db-autosync-mode-hook #'jp/org-roam-refresh-agenda-list)

(setq org-todo-keywords '(
                          (sequence "TODO(t)" "EPIC(e)" "PROJ(p)" "|"
                                "DONE(d)")
                          (sequence "BACKLOG(b)" "NEXT(n)" "PLAN(P)" "ACTIVE(a)"
                                    "REVIEW(r)" "WAIT(W@/!)" "HOLD(h)" "|"
                                    "COMPLETED(c)" "KILL(k)" "CANCELLED(C)" "STOPPED(s@)")
                        )
      )

(setq org-capture-templates '(
                              ("a" "Agenda")
                              ("ah" "Programming" entry (file+headline "~/share/org/Agenda.org" "Programming")
                               "* TODO %?\n %i\n %a")
                              ("ai" "Important" entry (file+headline "~/share/org/Agenda.org" "Important")
                               "* TODO %?\n %i\n %a")
                              ("as" "Sys" entry (file+headline "~/share/org/Agenda.org" "Sys")
                               "* TODO %?\n %i\n %a")
                              ("f" "Fleeting Note" entry (file+headline "~/org/Notes.org" "Tasks")
                               "* %?\n %x\n %i\n %a")
                              ("M" "Meeting" entry
                               (file+olp+datetree "~/share/org/Meetings.org")
                               (function jp/read-meeting-template)
                               :clock-in :clock-resume
                               :empty-lines 1)
                              ("m" "Email Workflow")
                              ("mf" "Follow Up" entry (file+olp "~/share/org/Mail.org" "Follow Up")
                               "* TODO %a\n%?\n#+begin_quote\n%x\n#+end_quote")
                              ("mr" "Read Later" entry (file+olp "~/share/org/Mail.org" "Read Later")
                               "* TODO %a\n%?\n#+begin_quote\n%x\n#+end_quote%x")
                              ("l" "Logbook Entries")
                              ("ls" "Software" entry
                               (file+olp+datetree "~/share/org/Logbook.org")
                               "\n* %U %a%? :Software:"
                               :clock-in :clock-resume)
                              ("lh" "Hardware" entry
                               (file+olp+datetree "~/share/org/Logbook.org")
                               "\n* %U %a%? :Hardware:"
                               :clock-in :clock-resume)
                              ("lc" "Configuration" entry
                               (file+olp+datetree "~/share/org/Logbook.org")
                               "\n* %U %a%? :Configuration:"
                               :clock-in :clock-resume)
                              ("s" "Create Scripts")
                              ("ss" "shell" entry
                               (file+headline "~/share/org/scripts/%<%Y%m%d%H%M%S>.org" "Scripts")
                               (function jp/read-script-template)
                               :clock-in :clock-resume
                               :empty-lines 1)
                              ("f" "Fleeting Note" entry (file+headline "~/share/org/Notes.org" "Tasks")
                               "* %?\n %x\n %i\n %a")
                              ("p" "Privat" entry (file+datetree "privat.org.gpg")
                               "* ~%<%H:%M>~ - %?\n"
                               :time-prompt t
                               :unnarrowed t)
                              ("t" "Task Entries")
                              ("tt" "Todo Task" entry (file+headline "~/share/org/Notes.org" "Tasks")
                               "* TODO %?\n %i\n %a")
                              ("te" "Epic Task" entry (file+headline "~/share/org/Notes.org" "Epic")
                               "* EPIC %?\n %i\n %a")
                              ("ti" "New Idea" entry (file+headline "~/share/org/Notes.org" "Ideas")
                               "* IDEA %?\n %i\n %a")))

(setq org-roam-capture-templates
      '(("d" "default" plain
         "%?\n\nSee also %a.\n"
         :if-new (file+head
                  "%<%Y%m%d%H%M%S>-${slug}.org"
                  "${title}\n")
         :unnarrowed t)
        ("j" "Projects" plain
         (function jp/read-newproject-template)
         :if-new (file+head
                  "Projects/%<%Y%m%d%H%M%S>-${slug}.org"
                  "${title}\n")
         :clock-in :clock-resume
         :unnarrowed t
         )
        ("i" "Individuum / Persona" plain
         "%?\n\nSee also %a.\n"
         :if-new (file+head
                  "People/%<%Y%m%d%H%M%S>-${slug}.org"
                  "${title}\n")
         :unnarrowed t
         )
        ("l" "Literature")
        ("ll" "Literature Note" plain
         "%?\n\nSee also %a.\n* Links\n- %x\n* Notes\n"
         :if-new (file+head
                  "Literature/%<%Y%m%d%H%M%S>-${slug}.org"
                  "${title}\n")
         :unnarrowed t
         )
        ("lr" "Bibliography reference" plain
         "#+ROAM_KEY: %^{citekey}\n#+PROPERTY: type %^{entry-type}\n#+FILETAGS: %^{keywords}\n#+AUTHOR: %^{author}\n%?"
         :if-new (file+head
                  "References/${citekey}.org"
                  "${title}\n")
         :unnarrowed t
         )
        ("p" "PC" plain
         "%?\n\nSee also %a.\n"
         :if-new (file+head
                  "PC/%<%Y%m%d%H%M%S>-${slug}.org"
                  "${title}\n#+date: %U")
         :unnarrowed t
         )
        )
      )

(setq org-roam-capture-ref-templates '(
                                       ("r" "Reference" plain
                                        "%?\n\n* Citations\n#+begin_quote\n${body}\n#+end_quote"
                                        :if-new (file+head
                                                 "Literature/%<%Y%m%d%H%M%S>-${slug}.org"
                                                 "${title}\n#+date: %U\n")
                                        :unnarrowed t
                                        )
                                       ("l" "Literature References" plain
                                        "%?\n\n* Abstract\n#+begin_quote\n${body}\n#+end_quote"
                                        :if-new (file+head
                                                 "References/%<%Y%m%d%H%M%S>-${slug}.org"
                                                 "${title}\n#+date: %U\n#+ROAM_REF: ${ref}")
                                        :unnarrowed t
                                        :empty-lines 1)
                                       ("w" "Web site" entry
                                        :target (file+head
                                                 "Literature/%<%Y%m%d%H%M%S>-${slug}.org"
                                                 "${title}\n#+date: %U\n")
                                        "* %a :website:\n\n%U %?\n\n#+begin_quote\n%:initial\n#+end_quote")
                                       )
      )

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :if-new (file+head
                  "%<%Y-%m-%d>.org"
                  "%<%Y-%m-%d>\n[[roam:%<%Y-%B>]]\n")
         :kill-buffer t
         )
        ("j" "Journal entry" entry
         "* ~%<%H:%M>~ - Journal  :journal:\n\n%?\n\n"
         :if-new (file+head+olp
                  "%<%Y-%m-%d>.org"
                  "%<%Y-%m-%d>\n"
                  ("Journal"))
         :kill-buffer t
         )
        ("l" "Monthly Log" entry
         "* %?\n  %U\n  %a\n  %i"
         :if-new (file+head+olp
                  "%<%Y-%B>.org"
                  "%<%Y-%B>\n"
                  ("Log"))
         :kill-buffer t
         )
        ("m" "meeting" entry
         (file "~/.dotfiles/doom/.doom.d/templates/Meeting.org")
         :if-new (file+head+olp
                  "%<%Y-%m-%d>.org"
                  "%<%Y-%m-%d>\n[[roam:%<%Y-%B>]]\n"
                  ("Meetings")))
        ("r" "Review")
        ("rd" "Daily Review" entry
         (file "~/.dotfiles/doom/.doom.d/templates/daily-review.org")
         :target (file+head
          "%<%Y-%m-%d>.org"
          "%<%Y-%m-%d>\n[[roam:%<%Y-%B>]]\n"))
        ("rm" "Monthly Review" entry
         (file "~/.dotfiles/doom/.doom.d/templates/monthly-review.org")
         :if-new (file+head
                  "%<%Y-%B>.org"
                  "%<%Y-%B>\n"))))

(setq org-agenda-custom-commands
      '(("d" "Dashboard"
         ((agenda "" ((org-deadline-warning-days 20)))
          (todo "BACKLOG"
                ((org-agenda-overriding-header "Backlog Tasks")))
          (todo "ACTIVE" ((org-agenda-overriding-header "Active Tasks")))
          (todo "REVIEW" ((org-agenda-overriding-header "Active Reviews")))
          (todo "EPIC" ((org-agenda-overriding-header "Active Epics")))))

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

        ("D" "Dashboard 2"
         ((agenda "" ((org-deadline-warning-days 7)))
          (tags-todo "+PRIORITY=\"A\""
                     ((org-agenda-overriding-header "High Priority")))
          (tags-todo "+followup" ((org-agenda-overriding-header "Needs Follow Up")))
          (todo "NEXT"
                ((org-agenda-overriding-header "Next Actions")
                 (org-agenda-max-todos nil)))
          (todo "ACTIVE" ((org-agenda-overriding-header "Active Tasks")))
          (todo "EPIC" ((org-agenda-overriding-header "Active Epics")))
          (todo "BACKLOG"
                ((org-agenda-overriding-header "Backlog")
                 (org-agenda-max-todos 99)))
          (todo "REVIEW" ((org-agenda-overriding-header "Active Reviews")))))

        ("n" "Next Tasks"
         ((agenda "" ((org-deadline-warning-days 7)))
          (todo "NEXT"
                ((org-agenda-overriding-header "Next Tasks")))))))

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
        ("next" . ?n)
        ("followup" . ?f)
        ("recurring" . ?r)
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
   (scheme . t)
   (guile . t)
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

(setq org-roam-directory org-directory   ; Set org-roam directory
      org-roam-dailies-directory (jp/org-path "daily")
      org-attach-id-dir (jp/org-path ".attachments")
      org-id-locations-file (doom-path ".orgids")
      org-roam-completion-everywhere nil
      org-roam-completion-system 'default
      org-roam-db-location (doom-path "org-roam.db")
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

(setq org-noter-notes-search-path '("~/ZK/References"))

(setq org-agenda-window-setup 'current-window)
(setq org-agenda-span 'day)
(setq org-agenda-start-day "0d")
(setq org-agenda-start-with-log-mode t)
(setq org-agenda-log-mode-items '(closed clock status))

;; Make done tasks show up in the agenda log
(setq org-log-done 'time)
(setq org-log-into-drawer t)

(setq org-datetree-add-timestamp 'inactive)
(setq org-habit-graph-column 60)
(setq org-fontify-whole-heading-line t)

(setq org-columns-default-format "%20CATEGORY(Category) %65ITEM(Task) %TODO %6Effort(Estim){:}  %6CLOCKSUM(Clock) %TAGS")

  (add-hook 'org-timer-set-hook #'org-clock-in)

  (defun jp/get-todays-journal-file-name ()
    "Gets the journal file name for today's date"
    (interactive)
    (let* ((journal-file-name
             (expand-file-name
               (format-time-string "%Y/%Y-%2m-%B.org")
               (jp/org-path "Journal/")))
           (journal-year-dir (file-name-directory journal-file-name)))
      (if (not (file-directory-p journal-year-dir))
        (make-directory journal-year-dir))
      journal-file-name))


  (defun jp/on-org-capture ()
    ;; Don't show the confirmation header text
    (setq header-line-format nil)

    ;; Control how some buffers are handled
    (let ((template (org-capture-get :key t)))
      (pcase template
        ("jj" (delete-other-windows)))))

  (add-hook 'org-capture-mode-hook 'jp/on-org-capture)

  (setq org-capture-templates
    `(("t" "Tasks")
      ("tt" "Task" entry (file ,(jp/org-path "Inbox.org"))
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
      ("ts" "Clocked Entry Subtask" entry (clock)
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Journal Entries")
      ("je" "General Entry" entry
           (file+olp+datetree ,(jp/org-path "Journal.org"))
           "\n* %<%I:%M %p> - %^{Title} \n\n%?\n\n"
           :tree-type week
           :clock-in :clock-resume
           :empty-lines 1)
      ("jt" "Task Entry" entry
           (file+olp+datetree ,(jp/org-path "Journal.org"))
           "\n* %<%I:%M %p> - Task Notes: %a\n\n%?\n\n"
           :tree-type week
           :clock-in :clock-resume
           :empty-lines 1)
      ("jj" "Journal" entry
           (file+olp+datetree ,(jp/org-path "Journal.org"))
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           :tree-type week
           :clock-in :clock-resume
           :empty-lines 1)))

(provide 'org-workflow)
