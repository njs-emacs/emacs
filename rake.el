(setq rake-args-default
  `(
    (build-dir . ".b")
    (build-files . ("makefile" "Makefile"))
    (command . "make -k")
    (target . "")
    )
  )

(defvar rake-args nil)

(defun rake-get (tag) (alist-r-get rake-args tag))
(defun rake-put (tag value) (setq rake-args (alist-put rake-args tag value)))

(defun rake-find-directory ()
  (let* ((build (rake-get 'build-dir))
	 (path (locate-alternate-files-in-path
		(rake-get 'build-files)
		(list "."
		     (filename-concat rake-base build)
		      build
		      ".."
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

(defun rake-look-for-default-target ()
  (cond
   ((sx (bol) (rsf "\\(.*?\\):" (sxp (eol)))) (ms 1))
   )
  )

(defun rake** (&optional target build-dir command)
  (let* ((rake-args (alist-merge rake-args-default rake-args))
	 (save default-directory)
	 (build-dir (or build-dir (rake-get 'build-dir)))
	 (command (or command (rake-get 'command)))
	 (target (or target (rake-get 'target)))
	 )
    (setq build-dir (rake-find-directory))
    (cd build-dir)
    (compile (format "%s %s" command target))
    (cd save)
    )
  )

(defun rake* (&optional target build-dir command)
  (let ((rake-base (locate-up-file-directory "rake.el"))
	(rake-args rake-args)
	)
    (and rake-base (load-file (filename-concat rake-base "rake.el")))
    (rake** target build-dir command)
    )
  )

(defun rake-target (&optional target build-dir command)
  (interactive
    (list (read-from-minibuffer "Target: " (rake-look-for-default-target))
	  nil
	  nil))
  (rake* target build-dir command)
  )

(defun rake (&optional target build-dir command) (interactive)
  (rake* target build-dir command)
  )

(defun rake-clean (&optional build-dir command)
  (interactive)
  (cond
   ((y-or-n-p "Really make clean? ")
    (rake* "clean" build-dir command)
    )
   ((message "Good choice!"))
   )
  )

(global-set-key [f10] 'rake)
(global-set-key [M-f10] 'rake-target)
(global-set-key [C-f10] 'rake-clean)

