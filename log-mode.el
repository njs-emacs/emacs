(autoload 'jlink-log-mode "e:/drs/jlink-log-mode")

(defun log-find-true-mode ()
  (sx
   (bob)
   (cond
    ((rsf "SEGGER" (sxp (eol))) 'jlink-log-mode)
    (t 'fundamental-mode)
    )
   )
  )

(defun log-mode ()
  (let ((mode (log-find-true-mode)))
    (funcall mode)
    )
  )

