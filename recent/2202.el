(defun *2202-org-init ()
  (interactive)
  (org-capture-template-add "t" '("todo" entry (file+headline "" "Tasks") "* TODO %?\n  %T"))
;  (org-capture-template-add "x" '("todo" entry (file "") "* DONE %?\n  %u"))
  (org-capture-template-add "s" '("slogan" entry (file "slogans.org") "* %?"))
  (org-capture-template-add "p" '("projects" entry (file+headline "" "projects") "* %?"))
  (org-capture-template-add "n" '("notes" entry (file "") "* %? %^g"))
  (org-capture-template-add "i" '("interesting" entry (file "") "* %? %^g"))
  )

;(*2202-org-init)

;(apropos "org-capture-template-put")
