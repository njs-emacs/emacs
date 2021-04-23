(defvar minibuffer-dir-map nil "Minibuffer directory shortcut map")
(setq minibuffer-dir-map (make-sparse-keymap))

(defun minibuffer-reset (s)
  (bol)
  (kill-line 1)
  (insert s))

(defun minibuffer-dir-try () (interactive)
  (let* ((x (lookup-key minibuffer-dir-map (this-command-keys))))
    (cond
     (x (minibuffer-reset x))
     )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun minibuffer-dir-key (key d)
  (cond (d
	 (define-key minibuffer-local-completion-map key 'minibuffer-dir-try)
	 (define-key minibuffer-local-must-match-map key 'minibuffer-dir-try)
	 (define-key minibuffer-dir-map key d)
	 )
	(t
	 (define-key minibuffer-local-completion-map key nil)
	 (define-key minibuffer-local-must-match-map key nil)
	 )
	)
  nil
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun minibuffer-dir (k d)
  (let ((kk (format "\e%s" k))) (minibuffer-dir-key kk d))
  )

(defun minibuffer-dir-show () (interactive)
  (require 'which-key)
  (which-key-show-full-keymap 'minibuffer-dir-map)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; panic recovery

;(define-key minibuffer-local-completion-map [27 ?p] 'previous-history-element)
;(define-key minibuffer-local-must-match-map [27 ?p] 'previous-history-element)

