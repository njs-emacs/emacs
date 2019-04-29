(setq completion-ignored-extensions
  (append '(".err" ".obj" ".o32") completion-ignored-extensions)
  )

;;; fset 'get-buffer-window probably obsolete now.
;;; was needed because of problems with optional args

(defun get-buffer-window-any-frame (buf)
  (car (delete nil (mapcar '(lambda (x) (get-buffer-window buf x))
			   (filtered-frame-list 'identity)))))

