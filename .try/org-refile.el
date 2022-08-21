;;; current file only
(setq org-refile-targets
  '(
    (nil :maxlevel . 1)
    ))

(setq org-refile-targets
  '(
    (nil :level . 2)
    ))

(setq org-refile-targets
  (nconc 
    `(
      (,(buffer-file-name) :maxlevel . 1)
      )
      org-refile-targets))
