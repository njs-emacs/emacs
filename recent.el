(defun command-history-save (&optional name) (interactive)
  (setq name
    (or name
	(let* ((save-dir (format "%s/emacs/host/%s" local-home system-name))
	       (default-directory save-dir)
	       )
	  (make-directory save-dir t)
	  (expand-file-name (format-time-string "_%y%m%d.el")))))
  (save-excursion
    (set-buffer (find-file-noselect name))
    (end-of-buffer)
    (dolist (i command-history)
      (prin1 i (current-buffer))
      (insert "\n")
      )
    (save-buffer)
    )
  )
(add-hook 'kill-emacs-hook 'command-history-save)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun toggle-slashification-region (arg) (interactive "p")
  (cond
   ((eq arg 2) (replace-regexp "/" "\\\\" nil (region-beginning) (region-end)))
   ((eq arg 3) (replace-regexp "\\\\" "/" nil (region-beginning) (region-end)))
   ((let ((a (or (sx (rsf "/" (region-end))) 99999999))
	  (b (or (sx (rsf "\\\\" (region-end))) 99999999))
	  )
      (cond ((< a b) (toggle-slashification-region 2))
	    ((toggle-slashification-region 3))
	    )
      ))
   )
  )

(define-key global-map (control-key-vector ?z ?/) 'toggle-slashification-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rc-mode () (interactive)
  (setq major-mode 'rc-mode)
  (setq mode-name "rc")
  (setq grep-spec "*")
  (setq grep-flags "-in")
  (setq rgrep-flags "-in")
  )

(defun rc-eval () (w32-shell-execute-with-msg "open" (buffer-file-name)))

(add-mode "\\.rc$" 'rc-mode)
(put 'rc-mode 'eval-buffer-modal 'rc-eval)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun arg-expand (arg-names)
  (let* (args)
    (setq args
      (delete nil (mapcar '(lambda (x)
			     (let ((xx (eval x)))
			       (cond (xx (format "-%s=%s" x xx))))) arg-names)))
    (mconcat args " ")
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun today-make-directory (&optional arg) (interactive "P")
  (make-directory
   (format-time-string
    (cond 
     (arg "%y%m%d-%H%M%S")
     (t "%y%m%d"))))
  (cond ((eq major-mode 'dired-mode) (revert-buffer)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun comment-line-string () (interactive)
  (cond
   ((eq major-mode 'emacs-lisp-mode) ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
   ((eq major-mode 'perl-mode) "################################################################")
   ((or
     (eq major-mode 'c-mode)
     (eq major-mode 'c++-mode)
     )
    "/* ================================================================ */")
   (t "################################################################")
   )
  )

(defun comment-line-insert () (interactive)
  (bol)
  (insert (comment-line-string) "\n")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun print-it () (interactive)
  (sx
   (let ((s (bs (region-beginning) (region-end)))
	 (buf (get-buffer-create "*print*")))
     (set-buffer buf)
     (erase-buffer)
     (insert "\e(s0p16.67h8.5v0s0b0T")
     (insert s)
     (print-buffer)
     )
   )
  )

(defun filename-to-kill (arg)
  (interactive "p")
  (kill-new
   (cond
    ((= arg 64) (basename (buffer-file-name)))
    ((= arg 16) (format "%s:%s" (buffer-file-name) (line-number-at-pos (point))))
    ((= arg 4) (buffer-file-name))
    ((= arg 1) (file-name-nondirectory (buffer-file-name)))
    )
   )
  )

(defun dired-quick-dot (arg) (interactive "p")
  (dired default-directory)
  )

(defun toggle-tab-width () (interactive) "Toggle tab width between 4 and 8"
  (setq tab-width (if (eq tab-width 8) 4 8))
  (redraw-display)
  )

;(defun search-kill (&optional arg)
;  (re-search-forward (car kill-ring))
;  )

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(defun inf (&rest args)
  (insert (apply 'format args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun region-sort ()
  (cond ((> (point) (mark)) (exchange-point-and-mark)))
  )

(defun rect-paste (s)
  (interactive
    (list 
     (read-from-minibuffer "Rect Paste: " (car kill-ring) nil nil 'kill-ring))
    )
  (region-sort)
  (let ((start (point-marker))
	(end (mark-marker))
	)
    (while (< (point) end)
      (sx (insert s))
      (forward-line 1)
      )
    )
  )
	       
(defun string-cliph (s)
  (sms "\\s *\\(.*\\)" s 1)
  )

(defun string-clipt (s)
  (let ((i (string-match "\\s *$" s)))
    (substring s 0 i)
    )
  )

(defun string-clip (s)
  (sms "\\s *\\(.*\\)\\s *" s 1)
  )

(defun eval-buffer-init-region ()
  (bob)
  (eval-region
   (sxp (or (rsf "###<<<###" nil nil 0 nil) (bob)))
   (sxp (or (rsf "###>>>###" nil nil 0 t) (eob)))
   )
  )

(defun kill-paren () (interactive)
  (cond
   ((looking-at "\\s(")
    (sx (forward-sexp 1) (backward-delete-char 1))
    (delete-char 1)
    )
   ((looking-at "\\s)")
    (fc 1)
    (sx (forward-sexp -1) (delete-char 1))
    (backward-delete-char 1)
    )
   )
  )

(defun ts-file (pat) (interactive "sPattern: ")
  (let* ((p (string-parse pat "\\([^%]*\\)%\\(.*\\)" 1 2))
	 (pp (apply 'format "%s.*%s" p))
	 (list (file-name-all-completions "" "."))
	 (list (delete nil (mapcar '(lambda (x)
				      (cond ((string-match pp x) x))) list)))
	 
	 )
    (format "%s%s%s" (nth 0 p) (length list) (nth 1 p))
    )
  )

(defun cfind (arg &optional path)
  (compile
   (format "find %s %s 2>/dev/null | sed 's/$/:1:/'" (or path ".") arg))
  )

(setq completion-rank
  '(
    (".*\\.KTM$" nil (".*\\.P$"))
    )
  )

(defun member-string-match (key list)
  (member-if '(lambda (pat) (string-match pat key)) list)
  )

(defun assoc-string-match (key list)
  "Return member of LIST which has a car which string-mathes KEY"
  (assoc-if '(lambda (pat) (string-match pat key)) list)
  )

(defun completion-sort (a b)
  (let* ((rank-a (assoc-string-match a completion-rank))
	 (rank-b (assoc-string-match b completion-rank))
	 (a>b (member-string-match b (cadr rank-a)))
	 (a<b (member-string-match b (caddr rank-a)))
	 (b>a (member-string-match a (cadr rank-b)))
	 (b<a (member-string-match a (caddr rank-b)))
	 )
    (or a>b b<a)
    )
  )

(defun minibuffer-complete-preferred () (interactive)
  (let* ((s (buffer-string))
	 (list (sort (file-name-all-completions
		      (file-name-nondirectory s)
		      (file-name-directory s)) 'completion-sort))
	 )
    (delete-region (point-min) (point-max))
    (insert (concat (file-name-directory s) (car list)))
    )
  )

(defmacro global-rep (old new)
 (list 'sx '(bob) (list 'while (list 'rep old new))))

(defun bup (&optional depth) (backward-up-list (or depth (depth))))

(defun kill-line+ (n)
  (kill-line n)
  (while (looking-at "^\\s *$") (kill-line 1))
  )

(defun if-out (region &optional tag)
  (let ((tag (or (eval tag) ""))
	(start (car region))
	(end (cadr region)))
    (cond ((and start end)
	   (goto-char end)
	   (eol)
	   (insert (format "\n#endif /*)%s*/" tag))
	   (goto-char start)
	   (bol)
	   (insert (format   "#if 0  /*(%s*/\n" tag))
	   )))
  )

(defun string-at (&optional p)
 (sx
  (goto-char (nth 2 (pps)))
  (fx 1)
  (readc)
  ))

(defun str-to-sym () (interactive)
  (sx (kill-sexp 1)
      (let ((sa (read (car kill-ring)))
	    (sb ""))
	(insert (downcase (reps sb " " "-")))
	)))

(defun xman () (interactive) (manual-entry (x-get-cut-buffer)))

(defun show-result (arg) (interactive "P")
  (let ((fun (cond (arg (intern (format "show-%s" arg)))
		   ('prin))))
    (show (funcall fun eval-result)))
  )

(defun %S (x) (format "%S" x))

(defun buffer (&optional object)
  "Coerce OBJECT to a buffer type object.\n
If OBJECT is a buffer return it.
If OBJECT is nil, return the current buffer.
If OBJECT is a file name, open the file and return the resulting buffer."
  (cond
   ((null object) (current-buffer))
   ((bufferp object) object)
   ((and (fboundp 'processp) (processp object)) (process-buffer object))
   ((or (get-buffer object) (find-file-noselect object)))
   ))

(defun rename-buffer-as-backup (&optional buffer) (interactive)
  (let* ((old (buffer-object buffer)))
    (cond (old
	   (setq new (buffer-name (generate-new-buffer (buffer-name old))))
	   (kill-buffer new)
	   (save-excursion (set-buffer old) (rename-buffer new))
	   ))))

(defun next (prop)
  "Assume point is within a list expression of form (SYM . VALUE)
and VALUE is a member of the list which is stored in property PROP for SYM,
then replace VALUE with the value which follows it in the property list."
  (save-excursion
    (cond ((looking-at "("))
	  ((up-list -1)))
    (forward-char 1)
    (let* ((x (get (read (current-buffer)) prop))
	   (p (point))
	   (y (read (current-buffer)))
	   (r (memq y x))
	   (n (car (cdr r)))
	   (new (or n (car x))))
      (goto-char p)
      (kill-sexp 1)
      (prin1 new (current-buffer))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
(defmacro srpage (&rest body)
  `(save-restriction (narrow-to-page) ,@body)
  )

(defun qf (x &optional n oq cq)
  (cond ((rsf x)
	 (sx (goto-char (me n)) (insert (or cq "\"")))
	 (sx (goto-char (mb n)) (insert (or oq "\"")))
	 t))
  )

(defmacro sn (start end &rest body)
  `(save-restriction
     (narrow-to-region ,start ,end)
     (bob)
     ,@body
     ))

(defmacro sbx (buffer &rest body)
  `(let ((*buf (current-buffer)))
     (pop-to-buffer (or ,buffer (current-buffer)))
	   (prog1 (progn ,@body)
	     (pop-to-buffer *buf)
	     )
	   )
  )

(defun *sbx (buffer body)
  (let ((*buf (current-buffer)))
	(pop-to-buffer (or buffer (current-buffer)))
	(prog1 (eval body)
	  (pop-to-buffer *buf)
	  )
	)
  )

(defmacro sbx (buffer &rest body)
  (list '*sbx buffer (list 'quote (cons 'progn body)))
  )

(defin 'sbx)

(defmacro sxk (&rest body)
  `(prog1 (progn ,@body) (kill-buffer (current-buffer))))

(defin 'sxk)

(defun copy-sexp-as-kill (&optional arg) (interactive "P")
  (copy-region-as-kill (point) (sxp (fx (or arg 1))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
(defun map-buffer-lines* (body)
  (sx
   (bob)
   (let ((result) (ok t))
     (while ok
       (setq result (cons (eval body) result))
       (setq ok (or (zerop (fl 1))))
       )
     (nreverse result)
     )))

(defmacro map-buffer-lines (&rest body)
  `(map-buffer-lines* '(progn ,@ body))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun preferred-completion () (interactive)
  (let ((s (buffer-string))
	(h (eval minibuffer-history-variable))
	)
    (cond ((file-exists-p (concat s ".c")) (insert ".c"))
	  )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun retabify (n)
  (let ((tab-width n))
    (untabify (point-min) (point-max))
    )
  (tabify-buffer)
  )

(defun file-local-start ()
  (sx
   (bob)
   (cond ((rsf "###<<<###")
	  (eval-region
	   (point)
	   (or (sx (rsf "###>>>###") (mb 0)) (point-max))))))
  )

;;;

(defun map-lines (fun &optional lim)
  (setq lim (or lim (point-max)))
  (sx
   (let ((result) (ok t))
     (while (< (point) lim)
       (setq result (cons (funcall fun (bs$)) result))
       (fl 1)
       )
     (nreverse result)
     )))

(defmacro qm (k &rest body)
  (let* ((name (intern (format "-*qm*-%s" k)))
	 (fun  (apply 'list 'lambda nil '(interactive)
		      (list 'define-key 'global-map [f2] (list 'quote name))
		      body)))
    (define-key global-map k name)
    (fset name fun)
    nil)
  )

(defmacro qmq (k &rest body)
  (apply 'list 'qm
	 (concat "\M-q" (cond ((numberp k) (char-to-string k)) (k))) body)
  )

(defun insert-time-string () (interactive)
  (insert (format-time-string "%y%m%d%H%M%S " (current-time)))
  )

(defun truncate-lines-toggle () (interactive)
  (setq truncate-lines (not truncate-lines))
  (recenter)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key minibuffer-local-completion-map [select] 'minibuffer-complete-preferred)
(define-key minibuffer-local-completion-map "\M- "  'preferred-completion)

(define-key global-map (control-key-vector ?=) 'comment-line-insert)

(define-key global-map "\C-x\C-h" 'file-history-show)
(define-key global-map "\C-x\C-y" 'copy-sexp-as-kill)

; (define-key global-map [mode-line drag-mouse-1] 'mldrag-drag-mode-line)

(define-key global-map "\M-f\M-b" 'rename-buffer-as-backup)

(define-key global-map "\M-u\M-u" 'show-result)

(define-key global-map "\M-u\t" 'toggle-tab-width)

(defun-key "\M-ut" 'visit-tags-table)

(define-key global-map "\M-_" 'kill-paren)

(define-key global-map "\M-{" (ilambda (goto-char (ps-fun))))
(define-key global-map "\M-}" (ilambda (goto-char (pe-fun))))

;;;;;;;;;;;;;;;;
(define-key z-map (vector (control-key ?.)) 'dired-quick-dot)

(define-key z-map "\C-p" 'print-it)
(define-key z-map "\C-f" 'filename-to-kill)

(define-key z-map "\C-j" 'join-line)
(define-key z-map "\C-u" 'upcase-region)
(define-key z-map "\C-d" 'downcase-region)

(define-key z-map "\C-s" 'toggle-case-fold-search)

(define-key z-map "\C-l" 'font-lock-mode)
(define-key z-map "\C-k" 'find-kill-head)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; take over help key

(define-key global-map "\C-h" 'backward-delete-char-untabify)
(define-key global-map "\C-x\C-h" 'help)
(define-key global-map "\C-x\C-q" 'find-file)

(define-key global-map "\C-]" nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; not sure i like this
(cond ((boundp 'want-icicle)
       (let ((load-path (cons (format "%s/0812/icicles" user-emacs-home) load-path)))
	 (load-library "icicles")
	 )
       )
      )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun buffer-file-name-to-kill () (interactive)
  (let ((s (buffer-file-name)))
    (kill-new s)
    )
  )

(define-key global-map "\C-z\C-f" 'buffer-file-name-to-kill)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun assoc-re (pat list)
  (let* ((fun '(lambda (x) (string-match pat (car x))))
	 (e (member-if fun list))
	 )
    e)
  )

(defun filename-replace-suffix (suffix &optional name)
  (concat (basename name) suffix)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun copy-emacs-url () (interactive)
  (kill-new (format "emacs:%s??%s" (buffer-file-name) (line-number-at-pos)))
  )

(defun copy-org-link () (interactive)
  (fset 'zz `(lambda () (interactive)
	       (insert ,(format "[[file:%s::%d][]]" (buffer-file-name) (line-number-at-pos)))
	       (fc -2)))
  )

(define-key global-map (control-key-vector ?z ?c) 'copy-emacs-url)
(define-key global-map (kbd "C-z C-o") 'copy-org-link)
(define-key global-map (kbd "C-z C-z") 'zz)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun toggle-region-visible (start end)
  (add-text-properties start end `(invisible ,(not (invisible-p start))))
  )

(defun toggle-visible-between (s1 s2 &optional n1 n2)
  (apply 'toggle-region-visible (search-pattern-bounds s1 s2 n1 n2))
  )

(defun toggle-visible-between-tags (tag)
  (let* ((tag (or tag ""))
	 (s1 (format "/\\*\\s *%s\\s *{" tag))
	 (s2 (format "}\\s *%s\\s *\\*/" tag))
	 )
    (apply 'toggle-region-visible (search-pattern-bounds s1 s2))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun chelp () (interactive)
; (shell-command-on-region (mark) (point) (format "perl chelp.pl"))
 (shell-command-on-region (sxp (bol)) (sxp (eol)) (format "perl chelp.pl"))
 (other-window 1)
 (cond ((rsf "##here##") (bol) (kill-line 1)))
 )

(define-key global-map "\C-z\C-h" 'chelp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun line-comment-set-state (state)
  (sx (bol) (kill-regexp "#*") (and state (insert "#")))
  )

(defun toggle-radio () (interactive)
 (let ((tag (sx (bol) (find-match-string "#~R{\\(.*\\)}"))))
  (sx (bob)
   (while (rsf tag)
     (line-comment-set-state t)
     )
   )
  (line-comment-set-state nil)
  ))
(define-key global-map "\C-z\C-r" 'toggle-radio)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq w32-pass-lwindow-to-system nil)
(setq w32-lwindow-modifier 'super)

(setq w32-pass-rwindow-to-system nil)
(setq w32-rwindow-modifier 'super)

(setq w32-apps-modifier 'hyper)

(defmacro dq (key &rest body)
  `(define-key global-map ,key
     (quote (lambda () (interactive) ,@body))
     ))

(defmacro hyper-quick (key &rest body)
  `(define-key global-map (kbd ,(format "H-%s" key))
     (quote (lambda () (interactive) ,@body))
     ))

(defmacro super-quick (key &rest body)
  `(define-key global-map (kbd ,(format "s-%s" key))
     (quote (lambda () (interactive) ,@body))
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun frame-default-height (&optional arg) (interactive "p")
  (let ((v (alist-get default-frame-alist 'height)))
    (cond
     ((eq arg 4) (setq v (/ v 2)))
     ((eq arg 16) (setq v (/ v 4)))
     )
    (set-frame-height (selected-frame) v)
    )
  )

(defun frame-default-width (&optional arg) (interactive "p")
  (let ((v (or (alist-get default-frame-alist 'width) 80)))
    (cond
     ((eq arg 4) (setq v (* v 2)))
     ((eq arg 16) (setq v (/ v 2)))
     )
    (set-frame-width (selected-frame) v)
    )
  )

(define-key z-map "\C-h" 'frame-default-height)
(define-key z-map "\C-w" 'frame-default-width)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(defun rs () (interactive) (insert (prin (read-key-sequence "???"))))
;(define-key global-map [M-f1] 'rs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rsf-fold-maybe (pat limit fold)
  (let ((case-fold-search fold)) (rsf pat limit t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun command-history-save (&optional name) (interactive)
  (or name 
      (setq name
	(daily-date-path
	 (concat (downcase system-name) "-%y%m%d-%H%M%S" "-hist.el") nil t)))
  (save-excursion
    (set-buffer (find-file-noselect name))
    (end-of-buffer)
    (dolist (i command-history)
      (prin1 i (current-buffer))
      (insert "\n")
      )
    (save-buffer)
    )
  )
(add-hook 'kill-emacs-hook 'command-history-save)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun insert-file-timestamp () (interactive)
  (insert (format-time-string "%y%m%d-%H%M%S" (visited-file-modtime)))
  )

(define-key global-map [M-f7] 'insert-file-timestamp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun not-this-file ()
  (let ((src (sx (bob) (rsf "##generated##.*from [<'`\"]\\(.*?\\)[>'`\"]") (ms 1))))
    (cond
     ((y-or-n-p
       (format "Edit file '%s' instead ? " src)
       )
      (let ((new (find-file-noselect src)))
	(kill-buffer (current-buffer))
	(switch-to-buffer new)
	(top-level)
	t
	)
      )
     ((y-or-n-p
       "Do you want to clear this hook ? "
       )
      (setq first-change-hook nil)
      )
     (t
      (toggle-read-only)
      (top-level)
      )
     )
    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun smart-git-process-sentinel (proc state)
  (cond
   ((string-match "finished" state)
    (delete-process proc)
    )
   )
  )

(defun smart-git-here (&optional d) (interactive "DSmartGit on Repository: ")
  (or d (setq d default-directory))
  (let ((last (filename-directory-last d))
	(default-directory d)
	proc)
    (cond
     ((string= last ".git") (smart-git-here ".."))
     ((file-directory-p ".git")
      (setq proc (start-process
		  (format "git-%s" last)
		  nil
		  "D:/S/SmartGit6.5/bin/smartgithg.exe"
		  "--open"
		  d))
      (set-process-sentinel proc 'smart-git-process-sentinel)
     )
    )
    )
  )

(define-key global-map [M-f11] 'smart-git-here)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(highlight-lines-matching-regexp "open" nil)
;(apropos "process")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ns-org-load-hook ()
  (add-hook 'org-mode-hook 'turn-on-font-lock)
  (setq org-file-apps (append org-file-apps (list (cons t 'emacs))))
  (setq org-open-directory-means-index-dot-org nil)
  )

(add-hook 'org-load-hook 'ns-org-load-hook)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar alt-buffer-eval-function nil "Alternate buffer eval function")

(make-local-variable 'alt-buffer-eval-function)

(defun alt-buffer-eval () (interactive)
 (cond (alt-buffer-eval-function (funcall alt-buffer-eval-function)))
 )
(define-key global-map [C-f9] 'alt-buffer-eval)

