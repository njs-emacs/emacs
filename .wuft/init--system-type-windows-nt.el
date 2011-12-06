(setq completion-ignored-extensions
  (append '(".err" ".obj" ".o32") completion-ignored-extensions)
  )

(fset 'get-buffer-window
      (list 'lambda '(buf &optional frame)
	    (list 'funcall
		  (symbol-function 'get-buffer-window) 'buf (or 'frame t))))

(defun get-buffer-window-any-frame (buf)
  (car (delete nil (mapcar '(lambda (x) (get-buffer-window buf x))
			   (filtered-frame-list 'identity)))))

