(org-link-open-from-string "[[file:e:/.org/notes.org::61][notes.org]]")

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "e:/roam/almond")
  (org-roam-db-location "e:/roam/almond/almond.db")
  :bind (
	 ("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today)
	 )
  
  :config
  (setq org-roam-dailies-directory "journal")
;  (define-key global-map (kbd "H-r") org-roam-dailies-map)
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

; (apropos "org-roam-db")
; (apropos "org-roam.*db")
; (apropos "org-roam.*database")
; (define-key global-map (kbd "H-r") org-roam-dailies-map)

(setq org-roam-graph-executable "c:/program files/Graphviz 2.44.1/bin/dot")
