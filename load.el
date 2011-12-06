(defun loading-path (x)
  "This function returns a filename in the same directory as the file currently being
loaded, regardless of whether the buffer is being loaded or evalled."
  (let* ((f (cond ((boundp 'fullname) fullname) (buffer-file-name)))
	 (d (file-name-directory f))
	 )
    (filename-concat d x)
    )
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
(defun init-local-noload (dir)
  (or (string-match "[/\\]$" dir) (setq dir (concat dir "/")))
  (setq local-home (expand-file-name dir))
  (set-default 'default-directory local-home)
  (setq default-directory local-home)
  (cons-load-path (concat local-home "emacs"))
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

(defun init-local (dir) (interactive "DDirectory: ")
  (init-local-noload dir)
  (or
   (load (expand-file-name "_emacs") t t)
   (load (expand-file-name ".emacs") t t)
   (load (expand-file-name ".emacs.el") t t)
   )
  )

(defun local-file (name) (concat (expand-file-name local-home) name))
