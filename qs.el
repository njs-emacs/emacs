; NOT WORKING YET

(setq qs-map (make-sparse-keymap))
(setq qsp-map (make-sparse-keymap))
(define-key global-map "\M-s" qs-map)
(define-key isearch-mode-map "\C-q" qs-map)
(define-key isearch-mode-map [f8] qs-map)

(mdotimes (i 26)
  (let* ((c (char-to-string (1+ i))))
    (define-key qs-map c (make-sparse-keymap))
    ))
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;

(defun qsp-map-show () (interactive)
  (show (cat (flatten (map-dump qsp-map)) "\n"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun qs-select (arg) (interactive "P")
  (let* ((key (this-command-keys))
	 (pat))
    (setq key (substring key (cond (arg 2) (1))))
    (setq pat (eval (lookup-key qsp-map key)))
    (setq isearch-string pat)
    (setq isearch-regexp t)
    (isearch-repeat 'forward)
    ))

(defun qs-delete (key)
  (define-key qsp-map key nil)
  (setq qsp-map (keymap-prune qsp-map))
  (define-key qs-map key nil)
  (setq qs-map (keymap-prune qs-map))
  (define-key isearch-mode-map "\C-q" 'qs-map)
  )

(defun qs-define (key form &optional expand)
  (let ((key (eval key)))
    (cond (expand (setq form (filename-canonical form))))
    (and (numberp key) (setq key (char-to-string key)))
    (define-key qsp-map key form)
    (define-key qs-map key 'qs-select)
    (format "key %s mapped to %s" key form)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;

(qs-define "\C-w\C-i" "WM_INITDIALOG")
(qs-define "ab" "WM_INITDIALOG")

(define-key qs-map "\M-s\M-s" 'qsp-map-show)
(define-key isearch-mode-map [f7] 'qs-select)
