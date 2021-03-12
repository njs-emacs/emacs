(defun eval-buffer-forms (pat &optional n)
  (setq n (or n 1))
  (sx (bob)
      (while (rsf pat) (sx (eval (read (ms n))))))
  )

(defun eval-buffer-forms-ediff-enter ()
  (debug)
  "Evaluate buffer lisp forms identified as specific to entry into ediff."
  (interactive)
  (eval-buffer-forms "#%E\\+\\(.*?\\)#")
  )

(defun eval-buffer-forms-ediff-leave ()
  "Evaluate buffer lisp forms identified as specific to exit from ediff."
  (interactive)
  (eval-buffer-forms "#%E-\\(.*?\\)#")
  )

;;; (add-hook 'ediff-prepare-buffer-hook 'eval-buffer-forms-ediff-enter)
;;; (remove-hook 'ediff-prepare-buffer-hook 'eval-buffer-forms-ediff-enter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; example is 
