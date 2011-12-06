(setq completion-ignored-extensions
  (append '(".o" ".a") completion-ignored-extensions)
  )

(defun get-buffer-window-any-frame (buf)
  (get-buffer-window buf)
  )

(let ((have-x-windows nil))
  (cond
   (have-x-windows
    (setq x-select-enable-clipboard t)
    )
   )
  )
