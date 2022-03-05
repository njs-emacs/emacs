; renamed z.2202.el to put at end of loaded file list

(defun *2202-org-init ()
  (interactive)
  (org-capture-template-add "t" '("todo" entry (file+headline "" "Tasks") "* TODO %?\n  %T"))
;  (org-capture-template-add "x" '("todo" entry (file "") "* DONE %?\n  %u"))
  (org-capture-template-add "s" '("slogan" entry (file "slogans.org") "* %?"))
  (org-capture-template-add "p" '("projects" entry (file+headline "" "projects") "* %?"))
  (org-capture-template-add "n" '("notes" entry (file "") "* %? %^g"))
  (org-capture-template-add "i" '("interesting" entry (file "") "* %? %^g"))
  )

(*2202-org-init)

;(apropos "org-capture-template-put")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun *2202-perl-key-translation () (interactive)
; make A-| produce a copyright symbol
  (define-key key-translation-map  (kbd "A-|") "©")

; make copyright symbol a word constituent
  (modify-syntax-entry 169 "w")

; now we can create abbreviations and snippets with no conflicts
; but we wouldn't want it to be too hard to insert the copyright symbol
; A-| isn't very hard
; but we could create a key mapping to insert and expand the snippet

; this part is specific to needs
  (local-set-key (kbd "H-1") "©a	Z")
  )

;;; (symbol-file 'org-mode)
;;; load-file-name
