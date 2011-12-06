(require 'speedbar)

(speedbar-add-supported-extension ".doc")
(setq speedbar-fetch-etags-command "c:/emacs-20.6/bin/etags.exe")
(setq speedbar-tag-split-minimum-length 50)

(defun speedbar-sort-refresh () (interactive)
  (setq speedbar-sort-tags (not speedbar-sort-tags))
  (speedbar-contract-line)
  (speedbar-expand-line)
  )

(define-key speedbar-file-key-map "s" 'speedbar-sort-refresh)

(defun imenu-rebuild () (interactive)
  (imenu--cleanup)
  (setq imenu--index-alist nil)
  (imenu-update-menubar)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun speedbar-fetch-dynamic-etags (file)
  "For FILE, run etags and create a list of symbols extracted.
Each symbol will be associated with its line position in FILE."
  (let ((newlist nil))
    (unwind-protect
	(save-excursion
	  (if (get-buffer "*etags tmp*")
	      (kill-buffer "*etags tmp*"))	;kill to clean it up
	  (if (<= 1 speedbar-verbosity-level)
	      (speedbar-message "Fetching etags..."))
	  (set-buffer (get-buffer-create "*etags tmp*"))
	  (apply 'call-process speedbar-fetch-etags-command nil
		 (current-buffer) nil
		 (append speedbar-fetch-etags-arguments (list file)))
	  (goto-char (point-min))
;	  (if (<= 1 speedbar-verbosity-level)
;	      (speedbar-message "Fetching etags..."))
;	  (set-buffer (get-buffer-create "*etags tmp*"))
	  (let ((expr
		 (let ((exprlst speedbar-fetch-etags-parse-list)
		       (ans nil))
		   (while (and (not ans) exprlst)
		     (if (string-match (car (car exprlst)) file)
			 (setq ans (car exprlst)))
		     (setq exprlst (cdr exprlst)))
		   (cdr ans))))
	    (if expr
		(let (tnl)
		  (while (not (save-excursion (end-of-line) (eobp)))
		    (save-excursion
		      (setq tnl (speedbar-extract-one-symbol expr)))
		    (if tnl (setq newlist (cons tnl newlist)))
		    (forward-line 1)))
	      (speedbar-message
	       "Sorry, no support for a file of that extension"))))
      )
    (if speedbar-sort-tags
	(sort newlist (lambda (a b) (string< (car a) (car b))))
      (reverse newlist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(speedbar-add-supported-extension ".ktm")

(defun speedbar-parse-ktm-tag ()
  (save-excursion
    (let ((bound (save-excursion (end-of-line) (point)))
	  (buf (buffer-name)))
      (cond ((re-search-forward "function\\s-*\\(\\sw+\\)" bound t)
	     (buffer-substring-no-properties (match-beginning 1)
					     (match-end 1)))
	    (t nil))
      )))

(setq speedbar-fetch-etags-parse-list
  (cons '("\\.ktm\\'" . speedbar-parse-ktm-tag)
	speedbar-fetch-etags-parse-list))

