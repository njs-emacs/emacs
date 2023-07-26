;;(yas--define-parents 'mug-mode '(emacs-lisp-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq mug-install-directory "e:/borough/barnet")

(defun mug-visit-org-file ()
  (interactive)
  (find-file-other-window (filename-concat mug-install-directory "mug.org"))
  )

