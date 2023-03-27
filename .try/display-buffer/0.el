(defun display-buffer-jubb (buffer alist)
  (debug)
  (display-buffer--maybe-at-bottom buffer alist)
  )

(defun display-buffer-jabb (buffer alist)
;  (debug)
  (current-window)
  )

(defun display-buffer-jobb (buffer alist)
  (debug)
  )
(display-buffer "1.flom" nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq display-buffer-alist nil)

(setq display-buffer-alist
  `(;; Display *Help* buffer at the bottom-most slot
     ("*\\(Help\\|trace-\\|Backtrace\\|RefTeX.*\\)"
      (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window)
      (side . right)
      (slot . 0)
      (window-width . 0.33)
      (reusable-frames . visible))
;;;
     ("\\.flom$"
      (display-buffer-jubb display-buffer-jabb display-buffer-jobb)
      (pop-up-frame-parameters
        (width . 80)
        (left . 1.0)
        (fullscreen . fullheight)))
     ("^\\*info"
      (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-pop-up-frame)
      (pop-up-frame-parameters
        (width . 80)
        (left . 1.0)
        (fullscreen . fullheight)))
;;;
     ;; Open new edited messages in a right-hand frame
     ;; For this to close the frame, add
     ;; (add-hook 'wl-draft-kill-pre-hook 'quit-window)
     ;; (setq wl-draft-send-function
     ;;   (lambda (buffer kill)
     ;;     (let ((frame (window-frame (selected-window))))
     ;;       (prog1 (wl-draft-normal-send-func buffer kill)
     ;;         (delete-frame frame)))))
     ("\\(\\*draft\\*\\|Draft/\\)"
      (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-pop-up-frame)
      (pop-up-frame-parameters
        (width . 80)
        (left . 1.0)
        (fullscreen . fullheight)))
;;;

;;;;     ;; TeX output buffers to bottom, with 10 lines
;;;;     (,(jjgr-rx-mode-name "^\\(TeX Output\\|TeX\\)")
;;;;      (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window)
;;;;      (side . bottom)
;;;;      (slot . 0)
;;;;      (window-height . 10)
;;;;      (reusable-frames . visible))
;;;;

;;;
     ;; Display *BBDB* buffer on the bottom frame
     ("\\*BBDB"
      (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window)
      (side . bottom)
      (slot . 0)
      (window-height . 10)
      (reusable-frames . visible))

     ;; Split shells at the bottom
;;;
     ("^\\*e?shell"
      (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-below-selected)
      (window-min-height . 20)
      (reusable-frames . visible)
      )
     )
   )

Debugger entered: nil
  display-buffer-jubb(#<buffer a.flom> ((pop-up-frame-parameters (width . 80) (left . 1.0) (fullscreen . fullheight)) (inhibit-same-window)))
  display-buffer(#<buffer a.flom> (display-buffer-same-window (inhibit-same-window)))
  pop-to-buffer(#<buffer a.flom> (display-buffer-same-window (inhibit-same-window)) nil)
  pop-to-buffer-same-window(#<buffer a.flom>)
  find-file("e:/emacs/.try/display-buffer/a.flom" t)
  funcall-interactively(find-file "e:/emacs/.try/display-buffer/a.flom" t)
  command-execute(find-file)

Debugger entered: nil
  display-buffer-jabb(#<buffer a.flom> ((pop-up-frame-parameters (width . 80) (left . 1.0) (fullscreen . fullheight)) (inhibit-same-window)))
  display-buffer(#<buffer a.flom> (display-buffer-same-window (inhibit-same-window)))
  pop-to-buffer(#<buffer a.flom> (display-buffer-same-window (inhibit-same-window)) nil)
  pop-to-buffer-same-window(#<buffer a.flom>)
  find-file("e:/emacs/.try/display-buffer/a.flom" t)
  funcall-interactively(find-file "e:/emacs/.try/display-buffer/a.flom" t)
  command-execute(find-file)

(display-buffer-use-some-window (get-buffer "1.flom") nil)
(display-buffer-use-least-recent-window (get-buffer "1.flom") nil)

(display-buffer-at-bottom (get-buffer "1.flom") nil)

(display-buffer-below-selected (get-buffer "1.flom") nil)

(display-buffer-in-child-frame (get-buffer "1.flom") nil)
(display-buffer-pop-up-frame (get-buffer "1.flom") nil)
(window--display-buffer (get-buffer "1.flom") (current-window) 'frame)
