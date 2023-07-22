(defun help-visit-mode-org ()
  (interactive)
  (find-file-other-window (format "%s/.help/org/%s.org" user-emacs-home major-mode))
  )
