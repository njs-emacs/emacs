
; 230725-162045
(dregf "git" nil "month")
(dregf "org.*template" nil "ever")
(dregf "org.*agenda" nil "ever")
(apropos "org.*note")

;; [[file:e:/emacs/.try/org-capture.el::105][org-capture.el]]

(setq org-default-notes-file "230725.org")
(setq org-agenda-files (list org-default-notes-file))

(setq org-capture-templates
  `(
    ("n" "note" entry (file org-default-notes-file) "* %?\n")
    )
  )

org-capture-templates

  (setq org-capture-templates
        `(("b" "Basic task for future review" entry
           (file+headline "tasks.org" "Tasks to be reviewed")
           ,(concat "* %^{Title}\n"
                    ":PROPERTIES:\n"
                    ":CAPTURED: %U\n"
                    ":END:\n\n"
                    "%i%l")
           :empty-lines-after 1)
