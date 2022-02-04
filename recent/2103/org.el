(setq org-confirm-babel-evaluate nil)
(setq org-confirm-elisp-link-function nil)

(setq org-confirm-shell-link-not-regexp ".")
(setq org-link-search-must-match-exact-headline nil)

(add-hook 'org-mode-hook 'turn-on-font-lock)

(define-key org-mode-map (kbd "C-'") c-quote-map)

(defun org-capture-template-put (&rest args)
  (setq org-capture-templates 
    (apply 'alist-put org-capture-templates args))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-agenda-add-current-file () (interactive)
 "Add current file to agenda files."
  (add-to-list 'org-agenda-files (buffer-file-name))
  (message "%s added to org-agenda-files" (buffer-file-name))
  )

(defun org-agenda-remove-current-file () (interactive)
 "Remove current file from agenda files."
  (setq org-agenda-files (delete (buffer-file-name) org-agenda-files))
  (message "%s removed from org-agenda-files" (buffer-file-name))
  )
  
(defun org-agenda-switch-current-file () (interactive)
  "Add current file to agenda files, or remove if already present."
  (cond
   ((member (buffer-file-name) org-agenda-files)
    (org-agenda-remove-current-file))
   ((org-agenda-add-current-file))
   )
  )
  

(def-key org-mode-map (kbd "C-v C-a") 'org-agenda-switch-current-file)

(add-to-list 'org-file-apps '(directory . emacs))
