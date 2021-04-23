(defun loading-path (&optional x)
  "This function returns a filename in the same directory as the file currently being
loaded, regardless of whether the buffer is being loaded or evalled."
  (let* ((f (cond ((boundp 'fullname) fullname) (buffer-file-name)))
	 (d (file-name-directory f))
	 )
    (cond (x (filename-concat d x)) (d))
    )
  )

(defun loading-file ()
  "This function returns the best guess as to which file is being loaded or evalled"
  (cond ((boundp 'fullname) fullname) (buffer-file-name))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun load-in-place (f &optional noerror nomessage)
  "Load emacs lisp file FILE, setting default-directory temporarily to the directory
containing that file. NOERROR NOMESSAGE arguments are as for 'load' function."
  (let* ((f (expand-file-name f))
	 (default-directory (file-name-directory f)))
    (load f noerror nomessage t t)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar local-home nil "Active \"home\" directory, set by 'init-local")

(defun init-local-noload (dir)
  (or (string-match "[/\\]$" dir) (setq dir (concat dir "/")))
  (setq local-home (expand-file-name dir))
  (set-default 'default-directory local-home)
  (setq default-directory local-home)
  (cons-load-path-if-exists (concat local-home "emacs"))
  (cons-load-path-if-exists (concat local-home ".emacs"))
  (minibuffer-dir " " local-home)
  (qb-define (control-key-vector ?d ? ) local-home) 
  (setenv "LOCALHOME" local-home)
  (and (get-buffer "*scratch*")
       (save-excursion
	 (set-buffer "*scratch*")
	 (setq default-directory local-home)
	 )
       )
  )

(defun load-if-local (f)
  "Load FILE only if it exists in the current directory."
  (and (file-exists-p f)
       (not (file-directory-p f))
       (load (expand-file-name f))
       )
  )

(defun init-local (&optional dir) (interactive "DDirectory: ")
  (or dir (setq dir default-directory))
  (init-local-noload dir)
  (or
   (load-if-local "_emacs")
   (load-if-local ".emacs")
   (load-if-local ".emacs.el")
   )
  )

(defun local-file (name) (concat (expand-file-name local-home) name))

(defun load-file-in-directory (dir file)
  (let ((default-directory (expand-file-name dir)))
    (load-file file)
    )
  )

(defun load-file-in-directory (file)
  (let* ((dir (expand-file-name (file-name-directory file)))
	 (file (file-name-nondirectory file))
	 (default-directory dir)
	 )
    (load-file file)
    )
  )
