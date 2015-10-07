(fset 'ucr 'upcase-region)
(fset 'dcr 'downcase-region)

(fset 'ee 'ediff-files)
(fset 'eb 'ediff-buffers)
(fset 'ed 'ediff-directories)
(defun edd (a &optional b pat)
  (ediff-directories a (or b default-directory) (or pat "")))

(defun ebq () (interactive)
  (let ((buf1 (current-buffer))
	(buf2 (ow (current-buffer)))
	)
    (or (eq buf1 buf2) (ediff-buffers buf1 buf2))
    )
  )

(fset 'j 'just-one-space)
(defun ip (&rest args) (insert (apply 'format args)))
(fset 'qr 'query-replace-regexp)

(fset 'sprint 'prin1-to-string)

(fset 'in 'indent-according-to-mode)
(fset 'gc 'goto-char)
(fset 'gl 'goto-line)

(fset 'dk 'global-set-key)

(defun mm (point) (set-marker (make-marker) point))

(defun rr (s new) (and (rsf s nil t) (replace-match new)))

(fset 'swx (symbol-function 'save-window-excursion))
(fset 'sx (symbol-function 'save-excursion))

(defmacro sbx (buffer &rest body)
  `(save-excursion (set-buffer ,buffer) ,@body))

(defmacro sxp (&rest body)
  (cons 'sx (append body '((point)))))

(fset 'bol (symbol-function 'beginning-of-line))
(fset 'eol (symbol-function 'end-of-line))

(defun eob () (goto-char (point-max)))
(defun bob () (goto-char (point-min)))

(defun point^ () (sxp (bol)))
(defun point$ () (sxp (eol)))

(fset '-> (symbol-function 'forward-char))
(fset '<- (symbol-function 'backward-char))
(fset 'fc (symbol-function 'forward-char))
(fset 'bc (symbol-function 'backward-char))
(fset 'fx (symbol-function 'forward-sexp))
(defun dx (&optional n) (kill-sexp (or n 1)))
(defun dc (&optional n) (delete-char (or n 1)))
(fset 'bx (symbol-function 'backward-sexp))
(fset 'tx (symbol-function 'transpose-sexps))

(defun bl (&optional n) (forward-line (- (or n 1))))
(defun fl (&optional n) (forward-line (or n 1)))

(fset 'bdc (symbol-function 'backward-delete-char))
(fset 'bs (symbol-function 'buffer-substring-no-properties))
(defun bss (&optional start end)
  (buffer-substring (or start (point)) (or end (point-max))))
(defun bsp (length &optional start)
  (setq start (or start (point)))
  (buffer-substring start (+ start length)))
(defun bs$ () (buffer-substring (point^) (point$)))

(fset 're (symbol-function 'recursive-edit))

(defmacro ow (&rest body)
  (apply 'list 'save-window-excursion '(other-window 1) body))

(fset 'bti 'back-to-indentation)
(defun fti (&optional arg) (forward-to-indentation (or arg 0)))
(defun eoln (&optional rx n) (eol) (and (rsb (or rx "\\S ")) (gme n)))

(defun kcb () (kill-buffer (current-buffer)))

(fset 'xf 'expand-file-name)

(fset 'eb 'eval-buffer-modal)

(fset 'mb 'match-beginning)
(defun me (&optional n) (match-end (or n 0)))
(defun mb (&optional n) (match-beginning (or n 0)))
(fset 'ms 'match-string)
(fset 'sm 'string-match)
(fset 'mr 'match-region)
(fset 'sms 'string-match-string)
(fset 'fms 'find-match-string)

(defun rsexp () (list (point) (sxp (fx))))

(fset 'sf 'search-forward)
(fset 'sb 'search-backward)

(defun pps (&optional p)
 (parse-partial-sexp (point-min) (or p (point))))

(defun sort-copy (list pred) (sort (copy-sequence list) pred))

(defun current-window () (get-buffer-window (current-buffer)))
(defun top-window-p () (eq (nth 1 (window-edges)) 0))

(defmacro ilambda (&rest body)
  (list 'quote (apply 'list 'lambda nil '(interactive) body)))

(defmacro toggle-fun (var)
  (list 'ilambda (list 'set var (list 'not var))))

(defmacro idefun (name &rest body)
  (list 'fset (list 'quote name) (cons 'ilambda body)))

(defmacro defun-key (k &rest body)
  (list 'define-key 'global-map k (cons 'ilambda body)))

(defun copy-line-as-kill ()
  (copy-region-as-kill (point^) (point$))
  )
