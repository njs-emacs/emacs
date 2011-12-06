(defun minibuffer-reset (s)
  (bol)
  (kill-line 1)
  (insert s))

(setq minibuffer-dir-map nil)

(defun minibuffer-dir-try () (interactive)
  (let* ((k (char-to-string (logand (aref (this-command-keys) 0) 127))))
    (minibuffer-reset (eval (gget minibuffer-dir-map k)))
    )
  )

(defun minibuffer-dir (k d)
  (let ((kk (format "\e%s" k)))
    (cond (d
	   (define-key minibuffer-local-completion-map kk 'minibuffer-dir-try)
	   (define-key minibuffer-local-must-match-map kk 'minibuffer-dir-try)
	   (setq minibuffer-dir-map (gput minibuffer-dir-map k d))
	   )
	  (t
	   (define-key minibuffer-local-completion-map kk nil)
	   (define-key minibuffer-local-must-match-map kk nil)
	   )
	  )
    nil
    ))

;(define-key minibuffer-local-map "\M-\C-?"
;  '(lambda () (interactive)
;     (while (not (= (preceding-char) ?/)) (backward-delete-char 1))))

(defun minibuffer-show () (interactive)
  (let* ((e (cdr (lookup-key minibuffer-local-completion-map "\e")))
	 (ee (subset-if 
	      '(lambda (x) (and (stringp (cdr x)) (file-directory-p (cdr x))))
	      e)))
    (show (mdolist (i ee) (format "%c\t%s\n" (car i) (cdr i))))
    )
  )

(defun minibuffer-show () (interactive)
  (apply 'show (mdolist (i minibuffer-dir-map) (format "%s\t%s\n" (car i) (cdr i))))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; panic recovery

;(define-key minibuffer-local-completion-map [27 ?p] 'previous-history-element)
;(define-key minibuffer-local-must-match-map [27 ?p] 'previous-history-element)

