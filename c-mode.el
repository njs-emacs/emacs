(define-abbrev-table 'c-abbrev
  '(("l" "lParam")
    ("w" "wParam")
    ("cW" "(WPARAM)")
    ("cL" "(LPARAM)")
    ("L" "LPARAM")
    ("W" "WPARAM")
    ("LR" "LRESULT")
    )
  )
		  
(defun c-abbrev-expand () (interactive)
  (let* ((s (bs (sxp (fx -1)) (point)))
	 (x (abbrev-expansion s c-abbrev))
	 )
    (cond
     (x (bdc (length s))
	(insert x)
	)
     )
    )
  )

(load-standard "cc-mode")

(set-default 'c-basic-offset 4)
(set-default 'c-syntactic-indentation t)

(set-default 'c-electric-flag nil)

(set-default 'c-indent-level 0)
(set-default 'c-argdecl-indent 4)
(set-default 'c-label-offset -4)
(set-default 'c-brace-imaginary-offset 4)
(set-default 'c-continued-statement-offset 4)
(set-default 'c-tab-always-indent nil)
(set-default 'c-auto-newline nil)

(setq c-mode-grep-spec "*.[hH] *.[cCyYbB] *.ic")
(setq c-mode-grep-spec "*.[hH] *.[cCyYbB]")

;(setq c-mode-hook nil)

(defun ns-gen-c ()
  (let ((debug-on-error t))
    (sx
     (bob)
     (let ((end (or (sx (rsf "^/\\* H")) (point-max))))
       (while (rsf "/\\*:(")
	 (backward-char 1)
	 (eval (read (current-buffer)))
	 )
       )
     )
    )
  )

(defvar ns-c-mode-hook-run-already nil "Workaround to stop hook being called twice")
(set-default 'ns-c-mode-hook-run-already nil)
(make-local-variable 'ns-c-mode-hook-run-already)

(defun ns-c-mode-hook-once ()
  (modify-syntax-entry ?_ "w")
  (define-key c-mode-map "\M-p" 'c-abbrev-expand)
  (define-key c-mode-map ";" 'self-insert-command)
  (define-key c-mode-map "," 'self-insert-command)
  (define-key c-mode-map ":" 'self-insert-command)
  (define-key c-mode-map "{" 'self-insert-command)
  (define-key c-mode-map "}" 'self-insert-command)
  (define-key c-mode-map "\C-c/" 'comment-hide-toggle)
  (define-key c-mode-map "\C-ct" (toggle-fun truncate-lines))
  (define-key c-mode-map "\C-c\C-m" 'prepend-module)

  (define-key c-mode-map "\ee" nil)

  (setq truncate-lines t)
  (setq grep-spec c-mode-grep-spec)
  (add-hook 'before-save-hook 'emacs-read-hash-minus)
  (add-hook 'after-save-hook 'ns-gen-c nil t)

  (emacs-read-hash-plus)
  
  (modify-syntax-entry ?_ "w" c++-mode-syntax-table)

;  (buffer-ring-add)
  )

(defun ns-c-mode-hook ()
  (cond
   (ns-c-mode-hook-run-already)
   ((setq ns-c-mode-hook-run-already t)
    (ns-c-mode-hook-once)
    )
   )
  )

(add-hook 'c-mode-hook 'ns-c-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ns-c++-mode-hook-once ()
  (let ()
    (modify-syntax-entry ?_ "w" c++-mode-syntax-table)
    (remove-hook 'c++-mode-hook 'ns-c++-mode-hook-once)
    )
  )

(add-hook 'c++-mode-hook 'ns-c++-mode-hook-once)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c-fun-name () (interactive)
  (save-excursion
    (re-search-backward "^{")
    (re-search-backward "([A-Za-zA_,]*)[^\n;]*$")
    (re-search-backward "\\Sw\\(\\sw*\\)")
    (match-string 1)
    ))

(defun c-inside-fun-p ()
  (condition-case ()
      (save-excursion
	(while (and (backward-up-list 1) (not (bolp))))
	(bolp))
    (error nil)))

(defun c-beautify-file ()
  (bob)
  (while (zerop (forward-line 1))
    (c-indent-line)
    ))

(defun comment-wrap-region () (interactive)
 (let ((start (sx (and (rsf "/\\*" nil t) (match-beginning 0))))
       (end (sx (and (rsf "\\*/" nil t) (match-end 0)))))
  (and start (list start end))))

(defun comment-hide (on)
  (save-excursion
    (let (x)
      (bob)
      (setq selective-display on)
      (while (setq x (comment-region))
	(apply 'subst-char-in-region
	       (append x (if on (list ?\n 13) (list 13 ?\n)) (list t)))
	(goto-char (car (cdr x))))
      )))

(defun comment-hide-toggle () (interactive)
  (comment-hide (not selective-display)))

(defun prepend-module () (interactive)
  (kill-new (format "%s@%s" (basename) (car kill-ring)))
  )

(defun c-comment-match-regexp (pat)
  (concat "/\\*\\s *" pat "\\s *\\*/")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c-other-file-c (&optional name)
  "The other file for files with suffix .c"
  (or name (setq name (buffer-file-name)))
  (cond (name
	 (let ((name (concat (basename name) ".h")))
	   (or (file-if-exists name)
	       (file-if-exists name "h/")
	       (file-if-exists name "../h/")
	       (file-if-exists name "../")
	       )))
	)
  )


(defun c-other-file-h (&optional name)
  "The other file for files with suffix .h"
  (or name (setq name (buffer-file-name)))
  (cond (name
	 (let ((name (concat (basename name) ".c")))
	   (or (file-if-exists name)
	       (file-if-exists name "c/")
	       (file-if-exists name "../c/")
	       (file-if-exists name "../")
	       )))
	)
  )

(defun c-other-file (&optional name)
  (or name (setq name (buffer-file-name)))
  (cond
   ((string-match (file-name-suffix name) "\\.c") (c-other-file-c name))
   ((string-match (file-name-suffix name) "\\.h") (c-other-file-h name))
   )
  )

(file-class-linked-file-add 'c-mode '((other . c-other-file)
				      (header . c-other-file)
				      ))

(defun c-foo () (interactive)
  (setq c-electric-flag nil)
  )
