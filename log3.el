(make-variable-buffer-local 'buffer-comment-function)
(set-default 'buffer-comment-function 'buffer-comment-generic)

(setq buffer-comment-alist
  `(
    (emacs-lisp-mode . buffer-comment-lisp)
    (nl-mode . buffer-comment-lisp)
    (perl-mode . buffer-comment-perl)
    )
  )

(defun buffer-comment-generic (s)
  (let ((f (gget buffer-comment-alist major-mode)))
    (cond
     (f (funcall f s))
     (s)
     )
    )
  )

(defun buffer-comment-lisp (s) (format ";; %s" s))
(defun buffer-comment-perl (s) (format "## %s" s))
(defun buffer-comment-c (s) (format "// %s" s))

(defun buffer-comment (s)
  (funcall buffer-comment-function s)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(make-variable-buffer-local 'log-time-identifier-string-format)
(set-default 'log-time-identifier-string-format "%m%d_%H%M%S")

(make-variable-buffer-local 'log-time-string-format)
(set-default 'log-time-string-format "%y%m%d-%H%M%S")

(defun log-full-time-insert (&optional kill) (interactive "P")
  (setq log-time-string (format-time-string log-time-string-format))
  (insert log-time-string)
  (and kill (kill-new log-time-string))
  )

(make-variable-buffer-local 'log-short-time-insert-format)
(set-default 'log-short-time-insert-format "%H%M%S")

(defun log-short-time-insert (&optional kill) (interactive "P")
  (setq log-time-string (format-time-string log-short-time-insert-format))
  (insert log-time-string)
  (and kill (kill-new log-time-string))
  )

(global-set-key [f7] 'log-full-time-insert)
(global-set-key [C-f7] 'log-short-time-insert)

(defun file-id-create-modtime () (interactive)
  (let ((modtime (nth 5 (file-attributes (buffer-file-name)))))
    (buffer-comment
     (format "##id## %s \"%s\"\n"
	     (format-time-string "%y%m%d-%H%M%S" modtime)
	     (buffer-file-name)
	     )
     )
    )
  )

(defun file-id-create ()
  (buffer-comment
   (format "##id## %s \"%s\"\n"
	   (format-time-string "%y%m%d-%H%M%S")
	   (buffer-file-name)
	   )
   )
  )

(defun file-id-insert () (interactive)
  (insert (file-id-create))
  )

(defun file-id-insert-modtime () (interactive)
  (insert (file-id-create-modtime))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; this doesn't work but is an attempt to solve the problem of
; unambibuously identifying files across the network
; this fails because of the legacy method of using shared drive letters
; instead of unc paths
; HOWEVER - shared drive letters are still the most convenient way 
; of doing stuff. 
; The problem is that //ned/setup and n:/setup are the same
; location, but emacs has no way of knowing that (currently)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(defun file-name-unc (file)
;  (let ((f (downcase file)))
;    (cond
;     ((string-match "^//\\(\\w*\\)/\\(\\w+\\)" f)
;      (let* ((host (substring f (match-beginning 1) (match-end 1)))
;	     (share (substring f (match-beginning 2) (match-end 2)))
;	     )
;	f))
;     ((string-match "^\\(\\w\\):/" f)
;      (let ((drive (substring f (match-beginning 1) (match-end 1)))
;	    (host (downcase system-name))
;	    )
;	(format "//%s/%s/%s" host drive (substring file (match-end 0)))
;	))
;     )
;    )
;  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun before-save-check-id ()
;  (debug)
  (save-excursion
    (bob)
    (cond
     ((rsf "##noid##"))
     ((or 
       (rsf "^\\(.*?\\)##id##\\s *\\(\\S +\\)\\s \"\\(.*?\\)\"")
       (rsf "^\\(.*?\\)##id##\\s *\\(\\S +\\)\\s *\\(.*\\)\\s *$")
       )
      (let ((id-file (match-string-no-properties 3))
	    (file (buffer-file-name))
	    )
	(cond
	 ((file-name-identical id-file file))
	 (t
	  (cond
	   ((y-or-n-p "File id mismatch - add new id y/n? ")
	    (bol)
	    (insert (match-string-no-properties 1) (file-id-create))
	    )
	   )
	  )
	 )
	)
      )
     )
    )
  )

(add-hook 'before-save-hook 'before-save-check-id)
