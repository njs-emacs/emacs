; emacs:d:/E/Emacs/emacs/lisp/vc-hooks.el??936

(setq vc-disable-state nil)

(defun vc-disable (&optional arg) (interactive "p")
  (cond
   (vc-disable-state
    (mapcar '(lambda (x) (fset (car x) (cdr x))) vc-disable-state)
    (setq vc-disable-state)
    (add-hook 'find-file-hooks 'vc-find-file-hook t)
    (message "vc enabled")
    )
   (t 
    (setq vc-disable-state `(
			     (vc-after-save . ,(symbol-function 'vc-after-save))
			     (vc-before-save . ,(symbol-function 'vc-before-save))
			     ))
    (remove-hook 'find-file-hooks 'vc-find-file-hook)
    (defun vc-after-save ())
    (defun vc-before-save ())
    (message "vc disabled")
    )
  )
  )

(define-key vc-prefix-map "e" 'vc-ediff)
(define-key vc-prefix-map "x" 'vc-disable)

