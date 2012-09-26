(defun gface (fg &optional bg name)
  (or name (setq name (intern (format "gface-%s-%s" fg bg))))
  (let* ((face (make-face name))
	 )
    (and fg (set-face-foreground name fg))
    (and bg (set-face-background name bg))
    (set name face)
    name
    )
  )

;(gface "white" "DodgerBlue")
;(gface "black" "pink" 

; (list-colors-display)
