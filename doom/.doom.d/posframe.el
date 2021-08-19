;;; .doom.d/posframe.el -*- lexical-binding: t; -*-

;; On Mac platform, Emacs can't enter Mac's native full-screen mode,
;; otherwise it will cause white screen and left and right sliding
;; after the original full-screen property is also integrated when
;; `make-frame' is created.
;;
;; So set `ns-use-native-fullscreen' and `ns-use-fullscreen-animation'
;; first to prevent Emacs from using Mac's native full-screen mode.
;; Instead of switching to the separate full-screen workspace of the
;; Mac, the traditional full-screen mode, traditional full-screen
;; mode, will only be in full screen in the current workspace.
;;
;; This way to execute `make-frame' when closing the code or plugin,
;; there will be no bugs caused by the sliding of the Mac's separate
;; workspace.
;;
;; On the Mac platform, you can't use `set-frame-parameter' and
;; `fullboth' directly to set the full screen.  That would also cause
;; the Mac window manager to directly throw the Emacs window into a
;; separate workspace, thus producing the same bug for `make-frame'.
;;
;; Therefore, when starting, set Emacs to maximize the window state by
;; `set-frame-parameter' and `maximized', and then set it to full
;; screen state after 5 seconds.
;;
;; Mac will not move the Emacs window to a separate workspace, and
;; finally solve the problem that the native full-screen window on the
;; Mac platform causes the `make-frame' to slide left and right.

(setq ns-use-native-fullscreen nil)
(setq ns-use-fullscreen-animation nil)
(run-at-time "5sec" nil
             (lambda ()
               (let ((fullscreen (frame-parameter (selected-frame)
               'fullscreen)))
                 ;; If emacs has in fullscreen status, maximized
                 ;; window first, drag from Mac's single space.
                 (when (memq fullscreen '(fullscreen fullboth))
                   (set-frame-parameter (selected-frame)
                   'fullscreen 'maximized))
                 ;; Manipulating a frame without waiting for the
                 ;; fullscreen animation to complete can cause a
                 ;; crash, or other unexpected behavior, on macOS
                 ;; (bug #28496).
                 (sleep-for 0.5)
                 ;; Call `toggle-frame-fullscreen' to fullscreen emacs.
                 (toggle-frame-fullscreen))))
