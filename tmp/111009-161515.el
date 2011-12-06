(defun compilation-mode-font-lock-keywords ()
  "Return expressions to highlight in Compilation mode."
  (if compilation-parse-errors-function
      ;; An old package!  Try the compatibility code.
      '((compilation-compat-parse-errors))
    (append
     ;; make directory tracking
     (if compilation-directory-matcher
	 `((,(car compilation-directory-matcher)
	    ,@(mapcar (lambda (elt)
			`(,(car elt)
			  (compilation-directory-properties
			   ,(car elt) ,(cdr elt))
			  t t))
		      (cdr compilation-directory-matcher)))))

     ;; Compiler warning/error lines.
     (debug)
     (mapcar
      (lambda (item)
	(if (symbolp item)
	    (setq item (cdr (assq item
				  compilation-error-regexp-alist-alist))))
	(and item
	 (let ((file (nth 1 item))
	      (line (nth 2 item))
	      (col (nth 3 item))
	      (type (nth 4 item))
              (pat (car item))
	      end-line end-col fmt)
          ;; omake reports some error indented, so skip the indentation.
          ;; another solution is to modify (some?) regexps in
          ;; `compilation-error-regexp-alist'.
          ;; note that omake usage is not limited to ocaml and C (for stubs).
          (when (and (= ?^ (aref pat 0)) ; anchored: starts with "^"
                     ;; but does not allow an arbitrary number of leading spaces
                     (not (and (= ?  (aref pat 1)) (= ?* (aref pat 2)))))
            (setq pat (concat "^ *" (substring pat 1))))
	  (if (consp file)	(setq fmt (cdr file)	  file (car file)))
	  (if (consp line)	(setq end-line (cdr line) line (car line)))
	  (if (consp col)	(setq end-col (cdr col)	  col (car col)))

	  (if (functionp line)
	      ;; The old compile.el had here an undocumented hook that
	      ;; allowed `line' to be a function that computed the actual
	      ;; error location.  Let's do our best.
	      `(,pat
		(0 (save-match-data
		     (compilation-compat-error-properties
		      (funcall ',line (cons (match-string ,file)
					    (cons default-directory
						  ',(nthcdr 4 item)))
			       ,(if col `(match-string ,col))))))
		(,file compilation-error-face t))

	    (unless (or (null (nth 5 item)) (integerp (nth 5 item)))
	      (error "HYPERLINK should be an integer: %s" (nth 5 item)))

	    `(,pat

	      ,@(when (integerp file)
		  `((,file ,(if (consp type)
				`(compilation-face ',type)
			      (aref [compilation-info-face
				     compilation-warning-face
				     compilation-error-face]
				    (or type 2))))))

	      ,@(when line
		  `((,line compilation-line-face nil t)))
	      ,@(when end-line
		  `((,end-line compilation-line-face nil t)))

	      ,@(when (integerp col)
		  `((,col compilation-column-face nil t)))
	      ,@(when (integerp end-col)
		  `((,end-col compilation-column-face nil t)))

	      ,@(nthcdr 6 item)
	      (,(or (nth 5 item) 0)
	       (compilation-error-properties ',file ,line ,end-line
					     ,col ,end-col ',(or type 2)
					     ',fmt)
	       append))))))		; for compilation-message-face
      compilation-error-regexp-alist)

     compilation-mode-font-lock-keywords)))
