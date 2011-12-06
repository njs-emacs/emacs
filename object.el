(defun object-file-name (f) (concat (basename f) ".o"))
(defun object-exists-p (f) (file-exists-p (object-file-name f)))

(defun xy (name)
  (let ((buf (shell-buffer (format "lnm %s" name))))
    (set-buffer buf)
    (setq cfun-list nil)
    (eval-current-buffer)
    ))

