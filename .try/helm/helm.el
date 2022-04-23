;;;(helm-find-files-grep)
;;;(helm-find-files-1)
;;;(helm-find-files)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun helm-ace-customize ()
  (interactive)
  (let ()
     (define-key helm-map (kbd "C-'") 'ace-jump-helm-line)
     )
  )

(eval-after-load "helm" `(helm-ace-customize))

(defun helm-ace-customize-1 ()
;; or if using key-chord-mode
;; (eval-after-load "helm"
;;  '(key-chord-define helm-map "jj" 'ace-jump-helm-line))

;  (setq ace-jump-helm-line-style 'pre)
  (setq ace-jump-helm-line-style 'at)
;  (setq ace-jump-helm-line-style 'at-full)
;  (setq ace-jump-helm-line-style 'de-bruijn)
  (setq ace-jump-helm-line-background t)
;  (setq ace-jump-helm-line-default-action 'select)
  (setq ace-jump-helm-line-default-action 'persistent)
;  (setq ace-jump-helm-line-default-action 'nil)
  (setq ace-jump-helm-line-select-key ?e) ;; this line is not neeeded
  
;; Set the move-only and persistent keys
  (setq ace-jump-helm-line-move-only-key ?o)
  (setq ace-jump-helm-line-persistent-key ?p)

;; enable idle execution for `helm-mini'
  (ace-jump-helm-line-idle-exec-add 'helm-mini)

;; enable hints preview
  (ace-jump-helm-line-autoshow-mode +1)
;; use `linum-mode' to show
  (setq ace-jump-helm-line-autoshow-mode-use-linum t)
  )

(defun helm-yank-selection-char (arg)
  "Yank a single character from the current display selection."
  (interactive "P")
  (with-helm-alive-p
;    (debug)
    (let* ((current (helm-minibuffer-completion-contents))
	   (selection (format "%s" (helm-get-selection nil (not arg))))
	   (new (substring selection 0 (1+ (length current))))
	   )
      (kill-new new)
      (helm-set-pattern new))))

(put 'helm-yank-selection-char 'helm-only t)

(defun helm-yank-selection-word (arg)
  "Yank a word from the current display selection."
  (interactive "P")
  (with-helm-alive-p
    (let* ((current (helm-minibuffer-completion-contents))
	   (selection (format "%s" (helm-get-selection nil (not arg))))
	   rhs new
	   )
      (setq rhs (substring selection (length current)))
      (setq rhs
	(cond
	 ((string-match-string "\\Sw*\\sw+" rhs 0))
	 ((string-match-string "\\sw+" rhs 0))
	 )
	)
      (setq new (concat current rhs))
      (kill-new new)
      (helm-set-pattern new))))

(put 'helm-yank-selection-word 'helm-only t)

(define-key helm-map (kbd "C-c C-x") 'helm-yank-selection-word)
