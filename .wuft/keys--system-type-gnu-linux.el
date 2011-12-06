;;; these keys are x-windows keys
;;; not sure how a provide for telnet terminal 

(define-key global-map "\M-`" 'undo)

(global-set-key [insert] 'other-window)
(global-set-key [M-delete] 'kill-current-buffer)
(global-set-key [M-insert] 'bury-buffer)
(global-set-key [S-M-insert] 'unbury-buffer)
(global-set-key [C-M-insert] 'bury-other)

(global-set-key [f12] 'exec-macro)
(global-set-key [S-f12] 'command-history-mode)
(global-set-key [M-f12] 'repeat-complex-command)


(global-set-key [kp-subtract] 'goto-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mouse-bury-buffer (click) (interactive "e")
  (let* ((start (event-start click))
	 )
    (select-window (posn-window start))
    (bury-buffer)
    )
  )
    
(defun mouse-kill-buffer (click) (interactive "e")
  (let* ((start (event-start click))
	 )
    (select-window (posn-window start))
    (kill-buffer-quick)
    )
  )
  
(global-set-key [mode-line S-mouse-1] 'mouse-bury-buffer)
(global-set-key [mode-line M-mouse-1] 'mouse-kill-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(global-unset-key [C-down-mouse-1])
;;;(global-unset-key [C-drag-mouse-1])
;;;(global-unset-key [S-mouse-1])
;;;(global-unset-key [C-mouse-1])
;;;(global-unset-key [C-double-down-mouse-1])
;;;(global-unset-key [C-double-mouse-1])

;;;(global-unset-key [S-drag-mouse-1])
;;;(global-unset-key [S-down-mouse-1])
;;;(global-unset-key [S-double-down-mouse-1])
;;;(global-unset-key [S-double-mouse-1])
;;;(global-unset-key [S-triple-mouse-1])
;;;(global-unset-key [S-triple-down-mouse-1])

;;;[M-S-mouse-1]
;;;[C-M-mouse-1]
;;;[C-M-S-mouse-1]

(global-set-key [S-mouse-1] 'mouse-yank-at-click)
(global-unset-key [S-down-mouse-1])
