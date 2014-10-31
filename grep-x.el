; need a reliable regexp
; this one can't handle spaces in filenames

(defconst grep-regexp-alist
  '(
    ;; Rule to match column numbers is commented out since no known grep
    ;; produces them
    ;; ("^\\(.+?\\)\\(:[ \t]*\\)\\([1-9][0-9]*\\)\\2\\(?:\\([1-9][0-9]*\\)\\(?:-\\([1-9][0-9]*\\)\\)?\\2\\)?"
    ;;  1 3 (4 . 5))
    ;; Note that we want to use as tight a regexp as we can to try and
    ;; handle weird file names (with colons in them) as well as possible.
    ;; E.g. we use [1-9][0-9]* rather than [0-9]+ so as to accept ":034:"
    ;; in file names.
    ("^\\(\\S +?\\)\\(:[ \t]*\\)\\([1-9][0-9]*\\)\\2"
     1 3
     ;; Calculate column positions (col . end-col) of first grep match on a line
     ((lambda ()
	(when grep-highlight-matches
	  (let* ((beg (match-end 0))
		 (end (save-excursion (goto-char beg) (line-end-position)))
		 (mbeg (text-property-any beg end 'font-lock-face 'match)))
	    (when mbeg
	      (- mbeg beg)))))
      .
      (lambda ()
	(when grep-highlight-matches
	  (let* ((beg (match-end 0))
		 (end (save-excursion (goto-char beg) (line-end-position)))
		 (mbeg (text-property-any beg end 'font-lock-face 'match))
		 (mend (and mbeg (next-single-property-change mbeg 'font-lock-face nil end))))
	    (when mend
	      (- mend beg)))))))
    ("^Binary file \\(.+\\) matches$" 1 nil nil 0 1))
  "Regexp used to match grep hits.  See `compilation-error-regexp-alist'.")

