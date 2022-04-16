(defun file-bigger-than-file-p (a b)
  "return t if file A is bigger than file B. Directories are treated as zero length."
  (let* ((aa (file-attribute-size (file-attributes a)))
	 (bb (file-attribute-size (file-attributes b)))
	 )
    (> aa bb))
  )


(defun file-type-gt-file-p (f1 f2)
  "return t if the string which describes the type for file A is \"bigger\" than the same attribute of file B."
  (let ((ext1 (or (file-name-extension f1) ""))
	(ext2 (or (file-name-extension f2) "")))
    (if (equal ext1 ext2)
	(string< f1 f2)
      (string< ext1 ext2)))
  )

(defun helm-ff-sort-apply (files function)
  (let ((sorted (sort files function)))
    (cond
     (helm-ff-sort-reverse (nreverse sorted))
     (sorted)
     )
    )
  )

(define-advice helm-list-directory (:override (directory) add-sort-by-moob)
  (let* ((files (directory-files directory t directory-files-no-dot-files-regexp))
	 (group (seq-group-by #'file-directory-p files))
	 (sfiles
	  (pcase helm-ff-sort-method
	    ('nil
	     files)
	    ('newest
	     (helm-ff-sort-apply files #'file-newer-than-file-p))
	    ('biggest
	     (helm-ff-sort-apply files #'file-bigger-than-file-p))
	    ('type
	     (let-alist group
	       (nconc
		.t                            ; folders
		(helm-ff-sort-apply
		 .nil                         ; files
		 #'file-type-gt-file-p
		 )))))
	  )
	 )
    sfiles
    )
  )

(defun helm-ff-sort-refresh ()
  (helm-force-update (helm-get-selection nil helm-ff-transformer-show-only-basename))
  (message "Sorting by %s%s"  helm-ff-sort-method (if helm-ff-sort-reverse " (reversed)" ""))
  )

(defun helm-ff-sort (x)
  (interactive "SSort by: ")
  (unless (eq helm-ff-sort-method x)
    (setq helm-ff-sort-method x)
    (helm-ff-sort-refresh)
    )
  )

(defun helm-ff-sort-type ()
  (interactive)
  (helm-ff-sort 'type)
  )

(defun helm-ff-sort-newer ()
  (interactive)
  (helm-ff-sort 'newest)
  )

(defun helm-ff-sort-biggest ()
  (interactive)
  (helm-ff-sort 'biggest)
  )

(defun helm-ff-sort-off ()
  (interactive)
  (helm-ff-sort nil)
  )

(defun helm-ff-sort-reverse-toggle ()
  (interactive)
  (setq helm-ff-sort-reverse (not helm-ff-sort-reverse))
  (helm-ff-sort-refresh)
  )

(defvar helm-ff-sort-reverse nil)
(defvar helm-ff-sort-method nil)

(put 'helm-ff-sort-type 'helm-only t)
(put 'helm-ff-sort-off 'helm-only t)
(put 'helm-ff-sort-newer 'helm-only t)
(put 'helm-ff-sort-biggest 'helm-only t)
(put 'helm-ff-sort-reverse-toggle 'helm-only t)

(setq helm-ff-sort-prefix "C-c C-s")
(setq helm-ff-sort-prefix-map (make-sparse-keymap))

(define-key helm-find-files-map (kbd helm-ff-sort-prefix) helm-ff-sort-prefix-map)

(define-key helm-ff-sort-prefix-map (kbd "r") #'helm-ff-sort-reverse-toggle)

(define-key helm-ff-sort-prefix-map (kbd "x") #'helm-ff-sort-off)
(define-key helm-ff-sort-prefix-map (kbd "a") #'helm-ff-sort-newer)
(define-key helm-ff-sort-prefix-map (kbd "t") #'helm-ff-sort-type)
(define-key helm-ff-sort-prefix-map (kbd "s") #'helm-ff-sort-biggest)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(setq helm-ff-sort-method 'biggest)
;(setq helm-ff-sort-method 'newest)
;(setq helm-ff-sort-method 'type)

;

;;; (apropos "remove.*advice")
;;; (apropos "advice.*remove")
;;; (apropos "advice")
;;; 
;;; (advice-mapc 'print 'helm-list-directory)
;;; (advice-remove 'helm-list-directory 'helm-list-directory@add-sort-xxx)

(describe-keymap helm-find-files-map)
(which-key-show-full-keymap 'helm-find-files-map)
(describe-bindings

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-helm-buffer-if-live (&rest body)
  "Eval BODY inside `helm-buffer'."
  (declare (indent 0) (debug t))
  (let ((buffer (get-buffer (helm-buffer-get))))
    (cond
     ((buffer-live-p buffer)
      `(with-current-buffer ,buffer ,@body))
     )
    )
  )

(with-helm-buffer-if-live (+ 1 2))
(macroexpand '(with-helm-buffer-if-live (+ 1 2)))

(define-derived-mode helm-grep-mode
    special-mode "helm-grep"
    "Major mode to provide actions in helm grep saved buffer.

Special commands:
\\{helm-grep-mode-map}"
    (set (make-local-variable 'helm-grep-last-cmd-line)
         (with-helm-buffer-if-live helm-grep-last-cmd-line))
    (set (make-local-variable 'revert-buffer-function)
         #'helm-grep-mode--revert-buffer-function))

(defvar ploxa-history nil)

(defun ploxa ()
  (debug)
  (setq default-directory (read-from-minibuffer "Where: " nil nil nil 'ploxa-history))
  (message "default-directory is %s" default-directory)
  )

(add-hook 'helm-grep-before-init-hook 'ploxa)
(add-hook 'helm-grep-after-init-hook 'ploxa)
(add-hook 'helm-grep-mode-hook 'ploxa)

