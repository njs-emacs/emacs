(setq qi-map (make-sparse-keymap))
(setq qii-map (make-sparse-keymap))
(setq qi-key-alist nil)

(define-key global-map "\C-o" qi-map)

(mdotimes (i 26)
  (let* ((c (char-to-string (1+ i))))
    (define-key qii-map c (make-sparse-keymap))
    ))
		
(defun qi-delete (key)
  (define-key qii-map key nil)
  (setq qii-map (keymap-prune qii-map))

  (define-key b-map key nil)
  (setq qi-map (keymap-prune qi-map))

  (setq qi-key-alist (gput qi-key-alist key nil))

  (define-key global-map "\C-o" qi-map)
  )

(defun qi-define (key &rest form)
  (let ((key (eval key)))
    (and (numberp key) (setq key (char-to-string key)))
    (setq key (string-to-vector key))
    (define-key qii-map key form)
    (define-key qi-map key 'qi-insert)
    (setq qi-key-alist (gput qi-key-alist key form))
    (format "key %s mapped to %s" key form)
    ))

; {#~ bug - qi-key-alist has problems when strings are two characters 
; {#~ bug - conflicts regarding vectors and strings

(defun qi-insert* (key)
  (setq key (string-to-vector key))
  (let* ((form (lookup-key qii-map key))
	 (a (gget qi-key-alist key))
	 (s (flatten (mapcar 'eval a)))
	 )
    
    (insert (car s))
    (save-excursion (mapcar 'insert (cdr s)))
    ))

(defun qi-insert (arg) (interactive "P")
  (qi-insert* (substring (this-command-keys) (cond (arg 2) (1))))
  )

(defun qi-map-show-1 (binding)
  (format "%s" (flatten (mapcar 'eval binding)))
  )

(defun qi-map-show () (interactive)
  (show (cat (flatten (map-dump qii-map 'qi-map-show-1)) "\n"))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq buffer-mode-comment-alist
  `((emacs-lisp-mode . ("; "))
    (html-mode . ("<!-- " " -->"))
    (sql-mode . ("/* " " */"))
    (c-mode . ("/* " " */"))
    (perl-mode . ("#"))
    )
  )

(defun buffer-mode-comment ()
  (let* ((a (gget buffer-mode-comment-alist major-mode))
	 (s (mapcar 'eval a))
	 )
    (or s "")
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ?u map slot reserved for utilities
; ?g map slot reserved for generics

(define-key qi-map (control-key-vector ?u ?s) 'qi-map-show)

(qi-define (control-key-vector ?g ?/) '(buffer-mode-comment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun insert-car-regexp-search-ring (s)
  (interactive
    (list
     (read-from-minibuffer
      "Insert: "
      (car regexp-search-ring) nil nil `(regexp-search-ring . 1))
     ))
  (insert s)
  )

(define-key qi-map (kbd "C-s") 'insert-car-regexp-search-ring)
