(defun prot/window-dired-vc-root-left ()
  "Open root directory of current version-controlled repository or the present working directory with 'dired' and bespoke window parameters. This is meant as a proof-of-concept function, illustrating how to leverage window rules to display a buffer, plus a few concomitant extras."
  (interactive)
  (let ((dir (if (eq (vc-root-dir) nil)
		 (dired-noselect default-directory)
	       (dired-noselect (vc-root-dir)))))
    (display-buffer-in-side-window
     dir `((side . left)
	   (slot . 0)
	   (window-width . 0.4)
	   (window-parameters . ((no-other-window . t)
				(no-delete-other-windows . t)
				(mode-line-format . (" "
						     "%b"))))))
    (with-current-buffer dir
      (rename-buffer "*Dired-Side*")))
  (with-eval-after-load 'ace-window
    (when (boundp 'aw-ignored-buffers)
      (add-to-list 'aw-ignored-buffers "*Dired-Side*")))
  )

(prot/window-dired-vc-root-left)


(defun prot/make-frame-floating-with-current-buffer ()
  "Display the current buffer in a new floating frame.
This passes certain parameters to the newly created frame:
- use a different name than the default; - use a graphical frame;
- do not display the minibuffer.
The name is meant to be used by the external rules of my tiling window manager (BSPWM) to present the frame in a floating state."
  (interactive)
  (make-frame `((name . "my_float_window")
;		(window-system . x)
		(minibuffer . nil)
;		(minibuffer . only)
		(height . 30)
;		(left . (+ -70))
		(left . 70)
		(mouse-wheel-frame . ,(selected-frame))
		(undecorated . t)
;		(alpha . 50)
		(vertical-scroll-bars . nil)
		(left-fringe . 0)
		(right-fringe . 0)
		)
	      )
  )

(prot/make-frame-floating-with-current-buffer)

display-buffer-alist

(defun display-buffer-jubb (buffer alist)
  (debug)
  )

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
      (display-buffer-jubb)
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

‘display-buffer’ builds a list of action functions and an action
alist by combining any action functions and alists specified by

display-buffer-overriding-action
nil

display-buffer-alist

ACTION argument, 

display-buffer-base-action
nil

display-buffer-fallback-action
((display-buffer--maybe-same-window
  display-buffer-reuse-window
  display-buffer--maybe-pop-up-frame-or-window
  display-buffer-in-previous-window
  display-buffer-use-some-window
  display-buffer-pop-up-frame))


display-buffer-same-window -- Use the selected window.
display-buffer-reuse-window -- Use a window already showing the buffer.
display-buffer-in-previous-window -- Use a window that did show the buffer before.
display-buffer-use-some-window -- Use some existing window.
display-buffer-pop-up-window -- Pop up a new window.
display-buffer-below-selected -- Use or pop up a window below the selected one.
display-buffer-at-bottom -- Use or pop up a window at the bottom of the selected frame.
display-buffer-pop-up-frame -- Show the buffer on a new frame.
display-buffer-in-child-frame -- Show the buffer in a child frame.
display-buffer-no-window -- Do not display the buffer and have return nil immediately.

display-buffer
  Command: Display BUFFER-OR-NAME in some window, without selecting
           it.
  Properties: event-symbol-element-mask event-symbol-elements
              modifier-cache
display-buffer--maybe-at-bottom
  Function: (not documented)
display-buffer--maybe-pop-up-frame
  Function: Try displaying BUFFER based on ‘pop-up-frames’.
display-buffer--maybe-pop-up-frame-or-window
  Function: Try displaying BUFFER based on ‘pop-up-frames’ or
            ‘pop-up-windows’.
display-buffer--maybe-pop-up-window
  Function: Try displaying BUFFER based on ‘pop-up-windows’.
display-buffer--maybe-same-window
  Function: Conditionally display BUFFER in the selected window.
display-buffer--special-action
  Function: Return special display action for BUFFER, if any.
display-buffer-assq-regexp
  Function: Retrieve ALIST entry corresponding to BUFFER-NAME.
display-buffer-at-bottom
  Function: Try displaying BUFFER in a window at the bottom of the
            selected frame.
display-buffer-below-selected
  Function: Try displaying BUFFER in a window below the selected
            window.
display-buffer-in-atom-window
  Function: Display BUFFER in an atomic window.
display-buffer-in-child-frame
  Function: Display BUFFER in a child frame.
display-buffer-in-direction
  Function: Try to display BUFFER in a direction specified by ALIST.
display-buffer-in-previous-window
  Function: Display BUFFER in a window previously showing it.
display-buffer-in-side-window
  Function: Display BUFFER in a side window of the selected frame.
display-buffer-in-tab
  Function: Display BUFFER in a tab.
display-buffer-jubb
  Function: (not documented)
display-buffer-no-window
  Function: Display BUFFER in no window.
display-buffer-other-frame
  Command: Display buffer BUFFER preferably in another frame.
display-buffer-pop-up-frame
  Function: Display BUFFER in a new frame.
display-buffer-pop-up-window
  Function: Display BUFFER by popping up a new window.
display-buffer-record-window
  Function: Record information for window used by ‘display-buffer’.
display-buffer-reuse-mode-window
  Function: Return a window based on the mode of the buffer it
            displays.
display-buffer-reuse-window
  Function: Return a window that is already displaying BUFFER.
display-buffer-same-window
  Function: Display BUFFER in the selected window.
display-buffer-use-some-frame
  Function: Display BUFFER in an existing frame that meets a
            predicate.
display-buffer-use-some-window
  Function: Display BUFFER in an existing window.

* variables
display-buffer--action-custom-type
  Variable: Custom type for ‘display-buffer’ actions.
  Properties: variable-documentation risky-local-variable
display-buffer--action-function-custom-type
  Variable: Custom type for ‘display-buffer’ action functions.
  Properties: variable-documentation risky-local-variable
display-buffer--other-frame-action
  Variable: A ‘display-buffer’ action for displaying in another frame.
  Properties: variable-documentation risky-local-variable
display-buffer--same-window-action
  Variable: A ‘display-buffer’ action for displaying in the same
            window.
  Properties: variable-documentation risky-local-variable
display-buffer-alist
  User option: Alist of user-defined conditional actions for
               ‘display-buffer’.
  Properties: standard-value custom-type risky-local-variable
              custom-version variable-documentation custom-requests
display-buffer-base-action
  User option: User-specified default action for ‘display-buffer’.
  Properties: standard-value custom-type risky-local-variable
              custom-version variable-documentation custom-requests
display-buffer-fallback-action
  Variable: Default fallback action for ‘display-buffer’.
  Properties: variable-documentation risky-local-variable
display-buffer-function
  User option: If non-nil, function to call to handle
               ‘display-buffer’.
  Properties: standard-value custom-type variable-documentation
              custom-requests byte-obsolete-variable
display-buffer-mark-dedicated
  Variable: If non-nil, ‘display-buffer’ marks the windows it creates
            as dedicated.
  Properties: variable-documentation
display-buffer-overriding-action
  Variable: Overriding action for buffer display.
  Properties: variable-documentation risky-local-variable
display-buffer-reuse-frames
  User option: Non-nil means ‘display-buffer’ should reuse frames.
  Properties: standard-value custom-type custom-version
              variable-documentation custom-requests
              byte-obsolete-variable
