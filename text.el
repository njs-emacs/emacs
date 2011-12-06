(load-standard "text-mode")

(setq auto-mode-alist (cons '("\\.doc$" . text-mode) auto-mode-alist))

(setq t-map (make-sparse-keymap))

(define-key t-map "s" 'spell-buffer)
(define-key t-map "f" 'fill-region)
(define-key t-map "k" 'kill-rectangle)
(define-key t-map "y" 'yank-rectangle)

(define-key t-map "l" 'set-left)
(define-key t-map "t" 'n-text)

(define-key t-map "c" 'force-text-column)

(define-key global-map "\et" t-map)

(define-key global-map "\C-x\C-y" 'line-copy)

(defun line-length ()
  "Return the length of the current line, not including the newline character."
  (-
   (save-excursion (end-of-line) (point))
   (save-excursion (beginning-of-line) (point))
   ))

(defun line-copy (arg)
  "Copy ARG (default 1) lines, without using kill or changing point."
  (interactive "p")
  (save-excursion
    (beginning-of-line)
    (insert (buffer-substring (point) (progn (forward-line arg) (point))))))

(defun line-string (&optional keep)
  "Return current line of current buffer as a string. Optional KEEP if non-nil,
save the end-of-line character"
  (buffer-substring (save-excursion (beginning-of-line) (point))
		    (save-excursion (if keep (forward-line 1) (end-of-line))
				    (point))))

(defun line-contains (re)
  "Return t if current line contains the regular expression REGEXP.

\".\"	identifies null lines.
\"\\S \"	identifies lines containing only white space.\n\n\n"
  (save-excursion
    (beginning-of-line)
    (re-search-forward re (save-excursion (end-of-line) (point)) t)))

(defun for-lines (p start end f &optional index)
  "For each line satisfying PREDICATE expression between START and END,
call FUNCTION at the beginning of that line.
If INDEX is non-nil, pass an index to FUNCTION, starting at zero.
\n\n\n"
  (save-excursion
    (goto-char start)
    (let ((i 0) (m (make-marker)))
      (move-marker m end)
      (while (< (point) (marker-position m))
	(beginning-of-line)
	(if (eval p) (progn
	      (if index (funcall f i) (funcall f))
	      (setq i (1+ i))))
	(forward-line 1)
	))))

(defun for-lines-matching (re start end f &optional index) 
  "For each line containing REGEXP between START and END,
call FUNCTION at the beginning of that line.
If INDEX is non-nil, pass an index to FUNCTION, starting at zero.
\n\n\n"
  (for-lines '(line-contains re) start end f index))

(defun lines-matching-concat (re)
  "Return a concatenation of all the lines containing REGEXP\n\n\n"
  (let ((tmp "")
	(f '(lambda () (setq tmp (concat tmp (line-string) "\n")))))
    (for-lines-matching re (point-min) (point-max) f)
    tmp))

(defun lines-matching-list (re)
  "Return a concatenation of all the lines containing REGEXP\n\n\n"
  (let (tmp
	(f '(lambda () (setq tmp (cons (line-string) tmp)))))
    (for-lines-matching re (point-min) (point-max) f)
    (reverse tmp)))

(defun buffer-line-strings ()
  "Return all the lines in the current buffer as a list of strings."
  (let (tmp)
    (save-excursion
      (end-of-buffer)
      (while (= (forward-line -1) 0)
	(setq tmp (cons (line-string) tmp))) tmp)))

(defun insert-lines-matching (re) (interactive "sRegexp: ")
  (insert (lines-matching-concat re)))
(define-key t-map "i" 'insert-lines-matching)

(defun force-column (col)
"Force non-blank text following COLUMN to start at COLUMN.\n\n\n"
  (move-to-column col)
  (delete-region (point) (rsf "\\s *"))
  (while (looking-at "[ 	]") (delete-char 1)))

(defun force-indent (col)
"Indent to COLUMN, then force following non-blank text to start at COLUMN.
To avoid (usually) unwanted symbol breaks, scan for non-blank first.\n\n\n"
  (while (looking-at "[ 	]") (forward-char 1))
  (indent-to col)
  (force-column col))

(defun force-text-column (s col) (interactive "sForce string: \nnColumn :")
"find TEXT, then force that text to start at COLUMN.\n\n\n"
  (re-search-forward s)
  (goto-char (match-beginning 0))
  (indent-to col)
  (force-column col))

(defun set-left () (interactive) (setq left-margin (current-column)))
(defun n-text () (interactive)
  (setq fill-column 70)
  (auto-fill-mode 1)
  (abbrev-mode 1)
  )
(setq text-mode-hook 'n-text)

(defun fill-indent () (interactive)
  (let ((fill-column (- fill-column left-margin))
	(p (point))
	(m (mark)))
    (fill-region p m)
    (indent-region p m left-margin)))

(define-global-abbrev "fro" "for")
