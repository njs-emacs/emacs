; this overrides dired not operating on '.' and '..'
;
;
;

(defun dired-get-filename (&optional localp no-error-if-not-filep)
  "In Dired, return name of file mentioned on this line.
Value returned normally includes the directory name.
Optional arg LOCALP with value `no-dir' means don't include directory
name in result.  A value of `verbatim' means to return the name exactly as
it occurs in the buffer, and a value of t means construct name relative to
`default-directory', which still may contain slashes if in a subdirectory.
Optional arg NO-ERROR-IF-NOT-FILEP means treat `.' and `..' as
regular filenames and return nil if no filename on this line.
Otherwise, an error occurs in these cases."
  (let (case-fold-search file p1 p2 already-absolute)
    (save-excursion
      (if (setq p1 (dired-move-to-filename (not no-error-if-not-filep)))
	  (setq p2 (dired-move-to-end-of-filename no-error-if-not-filep))))
    ;; nil if no file on this line, but no-error-if-not-filep is t:
    (if (setq file (and p1 p2 (buffer-substring p1 p2)))
	(progn
	  ;; Get rid of the mouse-face property that file names have.
	  (set-text-properties 0 (length file) nil file)
	  ;; Unquote names quoted by ls or by dired-insert-directory.
	  ;; Using read to unquote is much faster than substituting
	  ;; \007 (4 chars) -> ^G  (1 char) etc. in a lisp loop.
	  (setq file
		(read
		 (concat "\""
			 ;; Some ls -b don't escape quotes, argh!
			 ;; This is not needed for GNU ls, though.
			 (or (dired-string-replace-match
			      "\\([^\\]\\|\\`\\)\"" file "\\1\\\\\"" nil t)
			     file)
			 "\"")))
	  ;; The above `read' will return a unibyte string if FILE
	  ;; contains eight-bit-control/graphic characters.
	  (if (and enable-multibyte-characters
		   (not (multibyte-string-p file)))
	      (setq file (string-to-multibyte file)))))
    (and file (file-name-absolute-p file)
	 ;; A relative file name can start with ~.
	 ;; Don't treat it as absolute in this context.
	 (not (eq (aref file 0) ?~))
	 (setq already-absolute t))
    (cond
     ((null file)
      nil)
     ((eq localp 'verbatim)
      file)
;     ((and (not no-error-if-not-filep)
;	   (member file '("." "..")))
;      (error "Cannot operate on `.' or `..'"))
     ((and (eq localp 'no-dir) already-absolute)
      (file-name-nondirectory file))
     (already-absolute
      (let ((handler (find-file-name-handler file nil)))
	;; check for safe-magic property so that we won't
	;; put /: for names that don't really need them.
	;; For instance, .gz files when auto-compression-mode is on.
	(if (and handler (not (get handler 'safe-magic)))
	    (concat "/:" file)
	  file)))
     ((eq localp 'no-dir)
      file)
     ((equal (dired-current-directory) "/")
      (setq file (concat (dired-current-directory localp) file))
      (let ((handler (find-file-name-handler file nil)))
	;; check for safe-magic property so that we won't
	;; put /: for names that don't really need them.
	;; For instance, .gz files when auto-compression-mode is on.
	(if (and handler (not (get handler 'safe-magic)))
	    (concat "/:" file)
	  file)))
     (t
      (concat (dired-current-directory localp) file)))))
