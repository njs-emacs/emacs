(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/.roam")
  :bind (
	 ("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)
	 )
  
  :config
  (setq org-roam-dailies-directory "journal")
  (define-key global-map (kbd "H-r") org-roam-dailies-map)
  (org-roam-setup)
  )

; (apropos "org-roam.*map")

; (def-key global-map (kbd "C-c n l") 'org-roam-buffer-toggle)
; (def-key global-map (kbd "C-c n f") ' org-roam-node-find)
; (def-key global-map (kbd "C-c n i") ' org-roam-node-insert)
; (def-key global-map (kbd "C-c n TAB") 'completion-at-point)


; (setq org-roam-completion-everywhere t)

; org-roam-capture-templates

; (def-key global-map (kps "33") org-roam-dailies-map)

