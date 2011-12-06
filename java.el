(defun ns-java-mode-hook ()
  (let ((suffix (file-name-suffix (buffer-file-name))))
    (case-match
      suffix
      ("." (setq grep-spec (format "*%s" suffix)))
      )
    )
  )

(add-hook 'java-mode-hook 'ns-java-mode-hook)
