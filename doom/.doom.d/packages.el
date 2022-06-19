;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

;; Use latest version!
(unpin! org-roam org)
(unpin! bibtex-completion helm-bibtex ivy-bibtex)

;; Org Enhancements
(package! evil-tutor)           ; Tutor to get familiar with doom emacs (and evil vi keybindings).
(package! org-roam)             ; Extended org-mode for Zettelkasten principle.
(package! org-ref)              ; References and citations
(package! org-super-agenda)
(package! org-appear)           ; Display markup symbols (*=~ etc.) on cursor-over
(package! org-alert)            ; Enable org-mode notifications.
(package! org-tree-slide)       ; Enable org-mode presentations.
(package! org-modern)           ; Alternative for org-bullets and org-superstar.
(package! org-pdfview)          ; Allows to annotate pdf in org-mode.
(package! org-download)
(package! org-journal)
(package! org-sidebar)
(package! org-protocol-capture-html
  :recipe (:host github
           :repo "alphapapa/org-protocol-capture-html"
           :files ("org-protocol-capture-html.el")))
(package! org-special-block-extras
  :recipe (:host github
           :repo "alhassy/org-special-block-extras"))

;; Org Roam
(package! org-transclusion)
(package! vulpea)
(package! delve
  :recipe (:repo "publicimageltd/delve"
           :host github))

;; Bibliography
(package! org-roam-bibtex
  :recipe (:host github :repo "org-roam/org-roam-bibtex"))
(package! helm-bibtex)
(package! citar)

;; Org Roam UI (frontend for exploring and interacting org-roam)
(package! websocket)
(package! org-roam-ui
  :recipe (:host github
           :repo "org-roam/org-roam-ui"
           :files ("*.el" "out")))

;; PlantUML
(package! ob-napkin)            ; PlantUML in Org Babel
(package! plantuml-mode)        ; PlantUML Diagrams

;; Org Exports (ox-*)
(package! ox-twbs)      ; HTML Twitter Bootstrap
(package! ox-rst)       ; ReStructured Text (ReST, RST)

;; Support for other File Types
(package! pdf-tools)            ; Additional pdf tools.
(package! nov)                  ; View epub files.

;; (package! telega
;;   :recipe (:host github
;;            :repo "zevlg/telega.el"
;;            :branch "master"
;;            :files (:defaults "contrib" "etc" "server" "Makefile")))

(package! emacs-bitwarden       ; Password Manager
  :recipe (:host github
           :repo "seanfarley/emacs-bitwarden"
           :files ("bitwarden.el")))

; Visual Enhancements
(package! rainbow-mode)         ; Converts #0000FF and (nested (parethesis)) into colored cues.
(package! emojify)              ; Convert ☺ into emoji's.

;; Functional Enhancements
(package! popper)
(package! embark)

;; Other Software Integrations
(package! guix)
(package! dmenu)                ; Dmenu Plugin.
(package! tldr)                 ; Too long; Didn't read (short man pages).
(package! forge)                ; Additional git features (linking issues from github etc.)
(package! eshell-git-prompt)
(package! company-ledger)
(package! diminish)
(package! beacon)               ; Highlight Cursor on big change

;; Feed Reader
(package! elfeed)
(package! elfeed-score)

;; Thesaurus synonyms
                                        ;(package! synosaurus)
(package! calfw)
(package! calfw-org)
(package! helm-ag)              ; Ack and the_silver_searcher support

;; Dictionaries
;;(package! synosaurus)           ; Thesaurus synonyms
(package! powerthesaurus)

;; Language Server
(package! lsp-mode)
(package! lsp-ui)
(package! lsp-treemacs)
;;(package! lsp-ivy)
(package! lsp-pyright)          ; Python language server
(package! lsp-latex)
(package! dap-mode)             ; Debugging Functions
(package! company-box)          ; Auto-Completion
;; (package! pomm
;;   :recipe (:host github
;;            :repo "SqrtMinusOne/pomm.el"))

(package! beancount-mode
  :recipe (:host github
           :repo "beancount/beancount-mode"
           :files ("beancount.el")))

(package! mu4e-dashboard
  :recipe (:host github
           :repo "rougier/mu4e-dashboard"))

; Packages to share my keybindings when streaming
;;(package! command-log-mode)
(package! keycast
  :recipe (:host github
           :repo "tarsius/keycast"))

(package! emacs-rotate
  :recipe (:host github
           :repo "daichirata/emacs-rotate"))

;; MacOS Only
(package! spotlight)
