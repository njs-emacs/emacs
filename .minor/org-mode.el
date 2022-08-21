(defvar org-foo-keymap nil "Key map to use when org-foo-mode is active")
(setq org-foo-keymap (make-sparse-keymap))

(define-key org-foo-keymap (kbd "C-v C-c") 'org-refile)
(define-key org-foo-keymap (kbd "C-v C-v") 'org-archive-subtree)
(define-key org-foo-keymap (kbd "C-v C-e") `(lambda nil (interactive) (find-file ,(buffer-file-name))))


(define-minor-mode org-foo-mode
  "."
  :init-value nil
  :lighter " <F>"
  :keymap org-foo-keymap
  (cond 
   (org-foo-mode
    )
   (t
    )
   )
  )

