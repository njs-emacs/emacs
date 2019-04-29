;#~?emacs wrapper to findgrep.pl recursive search
;
; searches invoked from emacs will use this or a similar interface
; which invokes the compile method
; e:/perl/findgrep.pl
; e:/perl/lib/findgrep.pm

(defun *f* (cmd pat &optional fpat path max)
  (compile
   (format "perl -I%s/lib %s/findgrep.pl %s \"%s\" \"%s\" \"%s\" \"%s\""
	    perl-e-path
	    perl-e-path cmd pat
	    (or fpat "")
	    (or path "")
	    (or max "")
	    )))

(defun *fup (pat) (setq ff-last-pat (or pat (car regexp-search-ring))))

(defun fu (&optional pat &rest args)
  "Invoke findgrep(findunless) PAT FPAT PATH FARG MAX.
Show files not containing pattern"
  (apply '*f* "findunless" (*fup pat) args))

(defun ff (&optional pat &rest args)
  "Invoke findgrep(findgrep) PAT FPAT PATH FARG MAX"
  (apply '*f* "findgrep" (*fup pat) args))

(defun ff1 (&optional pat &rest args)
  "Invoke findgrep(findgrep) PAT FPAT PATH FARG MAX
Find first occurrence of pattern"
  (apply '*f* "findgrepf" (*fup pat) args))

(defun fg-first (pat &rest args)
  "Invoke findgrep(findgrep) PAT FPAT PATH FARG MAX
Find first occurrence of pattern"
  (apply '*f* "findgrepf" (*fup pat) args))

(defun ffe (pat &rest args) "alias for ff (emacs lisp files)" (interactive "Spattern: ") (apply 'ff pat "\\.el" args))
(defun ffl (pat &rest args) "alias for ff (nlisp files)" (interactive "Spattern: ") (apply 'ff pat "\\.nl" args))
(defun ffc (pat &rest args) "alias for ff (c source files)" (interactive "Spattern: ") (apply 'ff pat "\\.(c|h|ic|y)$" args))
(defun ffp (pat &rest args) "alias for ff (perl files)" (interactive "Spattern: ") (apply 'ff pat "\\.p[lm]$" args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
