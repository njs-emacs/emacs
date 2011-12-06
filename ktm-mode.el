(load "todo")

(defun ktm-other-file (&optional expand)
  (let ((file
	 (filename-replace-suffix
	  (cdr (car (assoc-re (file-name-suffix)
			      '((".KTM" . ".P") (".P" . ".KTM"))))))))
    (if expand (expand-file-name file) file)
    )
  )

(define-abbrev-table 'ktm-abbrev-table
  '(
    ("emd" "end")
    )
  )

(defun ktm-mode-font-lock ()
  (setq font-lock-defaults '(ktm-font-lock-keywords t t))
  (font-lock-mode)
  )

(setq ktm-font-lock-keywords
 '(
   ("function" . font-lock-keyword-face)
   ("^//!0\\S *" . todo-open-0-face)
   ("^//!1\\S *" . todo-open-1-face)
   ("^//!2\\S *" . todo-open-2-face)
   ("^//!3\\S *" . todo-open-3-face)
   ("^//!-\\S *" . todo-closed-face)
   ))

(defun ktm-mode () (interactive)
  (setq tab-width 3)
  (setq truncate-lines t)
  (setq major-mode 'ktm-mode)
  (setq mode-name "KTM")
  (setq grep-spec "*.[kK][tT][mMpP]")
  (setq grep-flags "-in")
  (setq rgrep-flags "-in")
  (setq case-fold-search t)
  (setq local-abbrev-table ktm-abbrev-table)
  (error "you need to fix the linked-file stuff"
  (abbrev-mode 1)
  (ktm-mode-font-lock)
  )

(setq compile-save-modes (cons 'ktm-mode compile-save-modes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; not sure if these should be common - but anyway not specific to ktm-mode
;

(defun grepv (pat &optional spec flags)
  (setq spec (or spec grep-spec))
  (setq flags (or flags grep-flags))
  (compile (format "egrep %s '%s' %s | egrep -v '//.*%s'"
		   flags pat spec pat))
  )

(defun greps (pat sort &optional spec flags)
  (setq spec (or spec grep-spec))
  (setq flags (or flags grep-flags))
  (compile (format "egrep %s '%s' %s | sort %s"
		   flags pat spec sort))
  )

(define-key compilation-mode-map "\C-c3"
  '(lambda () (interactive) (setq tab-width 3)
     (recenter)
     ))

