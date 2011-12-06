;; new bake

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bake-path-split ()
  (unconcat (getenv "PATH") ";")
  )
    
(defun bake-path-set (p)
  (setenv "PATH" (mconcat (mapcar 'slash-back p) ";"))
  )
    
(defun bake-path-delete (pat)
  (let ((p (bake-path-split)))
    (bake-path-set (delete-string-from-list-regexp pat p))
    ))
  
(defun bake-path-concat (list &optional insert-point after)
  (let ((p (bake-path-split)))
    (cond
     (insert-point (setq p (list-split-insert p list insert-point after)))
     (t (setq p (append p list)))
     )
    (bake-path-set p)
    ))

(defun bake-registered-path-list ()
  (apply 'append (mapcar '(lambda (x) (alist-get x 'path)) bake-tool-list))
  )

(defun bake-path-clean (p)
  (dolist (i (bake-registered-path-list))
    (setq p (delete i p))
    )
  p)

(setq bake-path-insertion-point "d:\\cygwin\\bin")

(defun bake-path-replace (paths)
  (let* ((p (unconcat (getenv "PATH") ";"))
	 path
	 )
    (setq p (bake-path-clean p))
    (setq p (list-split-insert p paths bake-path-insertion-point))
    (setenv "PATH" (setq path (mconcat p ";")))
    path
    )
  )
  
(setq bake-tool-list nil)
(setq bake-tool-tag nil)
(setq bake-tool-info nil)

(defun bake-get (tag) (alist-get bake-tool-info tag))
(defun bake-put (tag value) (setq bake-tool-info (alist-put bake-tool-info tag value)))

(defun bake-tool-register (tag info)
  (setq bake-tool-list (alist-put bake-tool-list tag info))
  )

(defun bake-tool-unselect ()
  (let* ((info (alist-get bake-tool-list bake-tool-tag))
	 (env (alist-get info 'env))
	 (fun (alist-get info 'unselect))
	 )
    (mapcar '(lambda (x) (setenv (car x) nil)) env)
    (and fun (funcall fun))
    )
  )
	 
(defun bake-tool-select (tag &rest args)
  (bake-tool-unselect)
  (setq bake-tool-tag tag)
  (setq bake-tool-info (alist-get bake-tool-list bake-tool-tag))
  (let* ((info bake-tool-info)
	 (fun (alist-get info 'select))
	 (env (alist-get info 'env))
	 )
    (bake-path-replace (alist-get info 'path))
    (mapcar '(lambda (x) (setenv (car x) (eval (cdr x)))) env)
    (and fun (funcall fun args))
    )
  )

(defun bake-find-directory ()
  (let* ((build (bake-get 'build))
	 (path (locate-alternate-files-in-path
		(bake-get 'build-files)
		(list "." ".." build
		      (format "../%s" build)
		      )
		)
	       ))
    (cond
     (path (file-name-directory path))
     ((error "cannot locate build in path"))
     )
    )
  )

(defun bake-find-command (&optional target)
  (let* ((command (bake-get 'command))
	 )
    (and target (setq command (format "%s %s" command target)))
    command
   )
  )

(defun bake-sanity-check ()
  (cond
   (bake-tool-tag)
   ((error "You need to call bake-tool-select"))
   )
  )

(defun bake-target* (&optional command target dir)
  (bake-sanity-check)
  (or dir (setq dir (bake-find-directory)))
  (or command (setq command (bake-find-command target)))
  (let ((save default-directory))
    (cd dir)
    (compile command)
    (cd save)
    )
  )

(setq bake-recursion-disable nil)

(defun bake-target (&optional command target dir) (interactive "STarget: ")
  (cond
   ((and (not bake-recursion-disable) (file-exists-p "bake.el"))
    (let ((bake-recursion-disable t))
      (load-file "bake.el")
      )
    )
   (t
    (bake-target* command target dir)
    )
   )
  )

(defun bake (&optional command dir) (interactive)
  (bake-target command dir)
  )

(defun bake-clean (&optional dir) (interactive)
  (bake-target nil "clean" dir)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro bake-save-select (tag &rest body)
  `(let ((save bake-tool-tag))
     (bake-tool-select ,tag)
     (prog1 (progn ,@body)
       (bake-tool-select save)
       )
     )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun null-bake-select (&optional build)
  (message "no bake now!")
  )

(setq null-bake-info
  `(
    (select . null-bake-select)
    )
  )

(bake-tool-register 'null null-bake-info)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun msvc-bake-select (&optional build)
  (default-frame-alist-put 'background-color "LightGray")
  (modify-frame-parameters (selected-frame) '((background-color . "LightGray")))
  )

(setq msvc-bake-info
  `((home . "h:/VS9/VC")
    (path . (
	     "h:\\VS9\\VC\\bin"
	     "h:\\VS9\\Common7\\ide"
	     ))
    (build . "__w")
    (build-files . ("makefile"))
    (command . "nmake -k")
    (env . (
	    ("INCLUDE" . "")
	    ("PERLLIB" . (expand-file-name (filename-concat devhome "__tool")))
	    ))
    (select . msvc-bake-select)
    )
  )

(bake-tool-register 'msvc msvc-bake-info)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun wcc-bake-select (&optional build)
  (default-frame-alist-put 'background-color "LightGray")
  (modify-frame-parameters (selected-frame) '((background-color . "LightGray")))
  )

(setq wcc-bake-info
  `((home . "d:\\w\\wcc")
    (path . (
	     "d:\\w\\wcc\\binx"
	     "d:\\w\\wcc\\binnt"
	     "d:\\w\\wcc\\binw"
	     ))
    (build . "__w")
    (build-files . ("make.pl" "makefile"))
    (command . "wmake -k")
    (env . (("MAKEFLAGS" . "-HUCE")
	    ("WATCOM" . "d:\\w\\wcc")
	    ("PERLLIB" . (expand-file-name (filename-concat devhome "__tool")))
	    ))
    (select . wcc-bake-select)
    )
  )

(bake-tool-register 'wcc wcc-bake-info)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun gcc-bake-select (&optional build)
  (or build (setq build
	      (case (wuft-get 'system-type)
		(gnu-linux ".bln")
		(windows-nt ".bwg")
		)
	      )
      )
  (and build (bake-put 'build build))
  )

(setq gcc-bake-info
  `(
    (path . (
	     ))
    (build . ".bwg")
    (build-files . ("makefile" "Makefile"))
    (command . "make -k")
    (env . (
	    
	    ))
    (select . gcc-bake-select)
    )
  )

(bake-tool-register 'gcc gcc-bake-info)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun generic-bake-select (&optional build)
  (and build (bake-put 'build build))
  )

(setq generic-bake-info
  `(
    (path . (
	     ))
    (build . ".")
    (build-files . ("makefile" "Makefile"))
    (command . "make -k")
    (env . (
	    ))
    (select . generic-bake-select)
    )
  )

(bake-tool-register 'generic generic-bake-info)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'bake)

(load "umake")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(bake-tool-select 'generic)
;(bake-tool-select 'wcc)
;(bake-tool-select 'gcc)
;(bake-tool-select 'umake)
;(bake-tool-select 'null)
;
;(bake-tool-select 'msvc)

;(path-show)

(setq bake-map (make-sparse-keymap))

(defun bake-define-key (key &optional command target dir)
  (define-key global-map key 'bake-bake)
  (define-key bake-map key (list command target dir))
  )

(defun bake-bake () (interactive)
  (let* ((keys (this-command-keys))
	 (b (lookup-key bake-map keys))
	 )
    (apply 'bake-target b)
    ))

(bake-define-key (kbd "H-a H-c") nil "squeaky" nil)
