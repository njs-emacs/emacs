(defun *2202-org-init ()
  (interactive)
  (org-capture-template-add "t" '("todo" entry (file+headline "" "Tasks") "* TODO %?\n  %T"))
  (org-capture-template-add "x" '("todo" entry (file "") "* DONE %?\n  %u"))
  )

;(*2202-org-init)

;(apropos "org-capture-template-put")
