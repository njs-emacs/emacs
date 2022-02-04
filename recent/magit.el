(defun magit-stage-this-file () (interactive)
  (magit-stage-file (buffer-file-name)))

(defun magit-here ()
  (interactive)
  (let* ((default-directory (locate-up-file-directory ".git")))
    (magit-status)
    )
  )
