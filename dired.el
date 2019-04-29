(load-standard "dired")

(defun funcall-existing-files (fun a b)
  (cond
   ((file-exists-p a)
    (cond ((file-exists-p b)
	   (funcall fun (xf a) (xf b)))
	  ((message "file b (%s) not found" b))))
   ((message "file a (%s) not found" a))
   ))
      
(defun dd-funcall (fun file)
  (let* ((file (file-name-nondirectory file))
	 (a (format "%s/%s/%s" (nth 0 dd) (nth 1 dd) file))
	 (b (format "%s/%s/%s" (nth 0 dd) (nth 2 dd) file))
	 )
    (cond
     ((eq fun 'exist) (and (file-exists-p a) (file-exists-p b)))
     ((funcall-existing-files fun a b))
     ))
  )

(defun dd-exists-p (file)
  (dd-funcall 'exist file)
  )

(defun dd-ediff (file)
  (dd-funcall 'ediff-files file)
  )

(defun dired-dd-ediff (file)
  (interactive (list (dired-get-filename 'no-dir)))
  (dd-ediff file)
  )

(defun dired-zip () (interactive)
  (let* ((file (dired-get-filename))
		 )
	(call-shell
	 (format "%s %s"
			 (cond ((string-match "\\.gz$" file) "gunzip")
				   ("gzip")
				   )
			 file))
	))

(defun substitute-file (format file)
  (let ((s "") (a (string-partition format "[^%]*%.")) (ca))
    (while a
      (setq ca (car a))
      (setq s
	(concat
	 s
	 (substring ca 0 -2)
	 (case (aref (substring ca -1) 0)
	   (?s file)
	   (?n (file-name-nondirectory file))
	   (?d (file-name-directory file))
	   (?b (basename file))
	   (?B (format "%s/%s/%s" (nth 0 dd) (nth 2 dd)
		       (file-name-nondirectory file)))
	   (?. (file-name-suffix file))
	   (?x (expand-file-name file))
	   )
	 ))
      )
    (setq a (cdr a))
    s))

(defun dired-mark-files-shell-command (command &optional marker-char)
  (interactive
   (list (dired-read-regexp (concat (if current-prefix-arg "Unmark" "Mark")
				    " files (command): "))
	 (if current-prefix-arg ?\040)))
  (let ((dired-marker-char (or marker-char dired-marker-char)))
    (dired-mark-if
     (and (not (looking-at dired-re-dot))
	  (not (eolp))			; empty line
	  (and (dired-get-filename nil t))
	  (let ((cmd (substitute-file command (dired-get-filename)))
		(result))
	    (message "executing %s..." cmd)
	    (setq result (call-process "sh" nil nil nil "-c" cmd))
	    (message "executing %s...%s" cmd result)
	    (sit-for 1)
	    (zerop result)
	    ))
     "matching file")))

(defun dired-mark-files-command (fun &optional marker-char)
  (interactive "xFunction: ")
  (let ((dired-marker-char (if current-prefix-arg ?\040 dired-marker-char))
	file)
    (dired-mark-if
     (and (not (looking-at dired-re-dot))
	  (not (eolp))			; empty line
	  (setq file (dired-get-filename nil t))
	  (funcall fun file)
	  )
     "matching file")))

(define-key dired-mode-map "%!" 'dired-mark-files-shell-command)
(define-key dired-mode-map "%x" 'dired-mark-files-command)
;(define-key dired-mode-map "\M-e" 'dired-dd-ediff)
(define-key dired-mode-map "\M-e" 'ediff-directories)
(define-key dired-mode-map "\M- " 'dired-mark)
(define-key dired-mode-map [M-mouse-1] 'dired-mark)

(load-overrides "dired")

(defun file-name-history-adjoin (file)
  (let ((file (expand-file-name file)))
    (setq file-name-history (delete file file-name-history))
    (setq file-name-history (cons (expand-file-name file) file-name-history))
    )
  )

(defun dired-find-file-add-history (fun file)
  (file-name-history-adjoin file)
  (funcall fun file)
  )

(defun dired-find-file ()
  "In dired, visit the file or directory named on this line."
  (interactive)
  (let ((file-name (file-name-sans-versions (dired-get-filename) t)))
    (if (file-exists-p file-name)
	(dired-find-file-add-history 'find-file file-name)
      (if (file-symlink-p file-name)
	  (error "File is a symlink to a nonexistent target")
	(error "File no longer exists; type `g' to update Dired buffer")))))

(defun dired-mouse-find-file-other-window (event)
  "In dired, visit the file or directory name you click on."
  (interactive "e")
  (let (file)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end event))))
      (save-excursion
	(goto-char (posn-point (event-end event)))
	(setq file (dired-get-filename))))
    (select-window (posn-window (event-end event)))
    (dired-find-file-add-history 'find-file-other-window (file-name-sans-versions file t))))

(defun dired-view-file ()
  "In dired, examine a file in view mode, returning to dired when done.
When file is a directory, show it in this buffer if it is inserted;
otherwise, display it in another buffer."
  (interactive)
  (if (file-directory-p (dired-get-filename))
      (or (and (cdr dired-subdir-alist)
	       (dired-goto-subdir (dired-get-filename)))
	  (dired (dired-get-filename)))
    (dired-find-file-add-history 'view-file (dired-get-filename))))

(defun dired-find-file-other-window ()
  "In dired, visit this file or directory in another window."
  (interactive)
  (dired-find-file-add-history 'find-file-other-window (file-name-sans-versions (dired-get-filename) t)))

(defun dired-display-file ()
  "In dired, display this file or directory in another window."
  (interactive)
  (let ((file (file-name-sans-versions (dired-get-filename) t)))
    (display-buffer (dired-find-file-add-history 'find-file-noselect file))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dired-mapcar-over-marks (fun)
  (interactive "aFunction: ")
  (let* ((list (dired-map-over-marks (cons (dired-get-filename) (point)) nil))
	 (lista (dired-get-marked-files))
	 r
	 )
    (setq r (mapcar fun lista))
    (revert-buffer)
    )
  )

(defun drm (file)
  (nvc-copy-for-deletion file)
  (delete-file file)
  )

(define-key dired-mode-map "r" 'dired-mapcar-over-marks)	       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dired-load-links () (interactive)
  (find-file ".links")
  )
(define-key dired-mode-map "YY" 'dired-load-links)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(setq ls-lisp-format-time-list '("%b %e %H:%M" "%b %e  %Y")) 
(setq ls-lisp-format-time-list '("%Y-%m-%d %H:%M" "%Y-%m-%d      ")) 
;(setq ls-lisp-format-time-list '("   %m-%d %H:%M" "%y-%m-%d      ")) 
;(setq ls-lisp-format-time-list '("%Y-%m-%d %H:%M:%S" "%Y-%m-%d %H:%M:%S")) 
;(setq ls-lisp-format-time-list '("%Y"))
(setq ls-lisp-use-localized-time-format t)
;(setq ls-lisp-use-localized-time-format nil)

(defun dired-subtree-customize () (interactive)
  (setq dired-subtree-use-backgrounds nil)
  (setq dired-subtree-map (make-sparse-keymap))
  (define-key dired-mode-map (kbd "C-c") dired-subtree-map)
  (let ((map dired-subtree-map))
    (define-key map "\C-v" 'dired-subtree-insert)
    (define-key map "\C-k" 'dired-subtree-remove)
    (define-key map "\C-i" 'dired-subtree-toggle)
    (define-key map "\C-c" 'dired-subtree-cycle)
    (define-key map "\C-z" 'dired-subtree-revert)
    (define-key map "\C-r" 'dired-subtree-narrow)
    (define-key map "\C-p" 'dired-subtree-up)
    (define-key map "\C-n" 'dired-subtree-down)
    (define-key map "\C-x\C-n" 'dired-subtree-next-sibling)
    (define-key map "\C-x\C-p" 'dired-subtree-previous-sibling)
    (define-key map "\C-f" 'dired-subtree-next-sibling)
    (define-key map "\C-h" 'dired-subtree-previous-sibling)
    (define-key map "\C-a" 'dired-subtree-beginning)
    (define-key map "\C-e" 'dired-subtree-end)
    (define-key map "\C-x\C-m" 'dired-subtree-mark-subtree)
    (define-key map "\C-x\C-u" 'dired-subtree-unmark-subtree)
    (define-key map "\C-x\C-a" 'dired-subtree-only-this-file)
    (define-key map "\C-x\C-s" 'dired-subtree-only-this-directory)
    )
  )

