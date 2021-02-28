(defun shell-line ()
  (interactive)
  (let ((cmd (bs (sxp (bol) (rsf "\\s-*")) (point$)))) (shell-command cmd))
  )

(defun kill-buffers-matching (fun)
  "Kill all buffers for which (FUN buffer) returns non-nil."
  (mapcar '(lambda (buf) (cond ((funcall fun buf) (kill-buffer buf)))) (buffer-list))
  )

(defun kill-buffers-by-name (pat)
  "Kill all buffers matching PATTERN."
  (interactive)
  (dolist (buf (buffer-list))
    (if (string-match-string pat (buffer-name buf)) (kill-buffer buf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun loading ()
  (catch 'done
    (let ((i 6) frame)
      (while (setq frame (backtrace-frame i))
	(cond
	 ((eq (cadr frame) 'load-with-code-conversion) (throw 'done t))
	 )
	(setq i (1+ i))
	)
      (throw 'done nil)
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(defun toggle-slashification (arg start end)
  (sx (goto-char start)
      (cond
       ((eq arg 2) (replace-regexp "/" "\\\\" nil start end))
       ((eq arg 3) (replace-regexp "\\\\" "/" nil start end))
       (
	(let ((a (or (sx (rsf "/" end)) 99999999))
	      (b (or (sx (rsf "\\\\" end)) 99999999))
	      )
	  (cond ((< a b) (toggle-slashification 2 start end))
		((toggle-slashification 3 start end))
		)
	  ))
       )
      )
  )

(defun toggle-slashification-region (arg)
  (interactive "p")
  (cond ((region-active-p) (toggle-slashification arg (region-beginning) (region-end)))
	((toggle-slashification arg (point^) (point$)))
	)
  )

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
      (delete nil (mapcar #'(lambda (x)
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
   ((eq major-mode 'nl-mode) ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
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
	 (list (delete nil (mapcar #'(lambda (x)
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

(define-key global-map "\C-x\C-y" 'copy-sexp-as-kill)

; (define-key global-map [mode-line drag-mouse-1] 'mldrag-drag-mode-line)

(define-key global-map "\M-f\M-b" 'rename-buffer-as-backup)

(define-key global-map "\M-u\M-u" 'show-result)

(define-key global-map "\M-u\t" 'toggle-tab-width)

(defun-key "\M-ut" 'visit-tags-table)

(define-key global-map "\M-_" 'kill-paren)

;;;;;;;;;;;;;;;;
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

(define-key z-map "C-f" 'buffer-file-name-to-kill)
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

(defun region-text ()
  (cond
   ((use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)))
   ))

(defun copy-org-link (&optional arg)
  (interactive "P")
  (let* ((r (region-text))
	 (line (line-number-at-pos))
	 (cap (or (region-text)
		  (cond
		   ((equal arg '(4))
		    (format "%s:%d" (file-name-nondirectory (buffer-file-name)) line))
		   ((equal arg '(1))
		    (format "%d" line))
		   (t
		    (format "%s" (file-name-nondirectory (buffer-file-name)))
		    )
		   )))
	 (link
	  (cond
	   ((use-region-p)
	    (format "file:%s::%s" (buffer-file-name) (or (region-text) line))
	    )
	   (t (format "file:%s::%s" (buffer-file-name) line))
	   )
	  )
	 )
    (kill-new (format "[[%s][%s]]" link cap))
    (message "copied '%s' to kill" link)
    (fset 'zz `(lambda () (interactive)
		 (insert ,(format "[[%s][" link))
		 (sx (insert ,cap "]"))
		 )
	  )
    )
  )

(define-key z-map (control-key-vector ?c) 'copy-emacs-url)
(define-key z-map (kbd "C-o") 'copy-org-link)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun copy-gdb-break (&optional arg)
  (interactive "P")
  
  (let* ((file (file-name-nondirectory (buffer-file-name)))
	 (cap (or (region-text) (format "%d" (line-number-at-pos))))
	 (break (format "break %s:%s" file cap))
	 )
    (kill-new break)
    (message "copied '%s' to kill" break)
    )
  )
(define-key z-map (kbd "C-v") 'copy-gdb-break)

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
(setq w32-pass-lwindow-to-system nil)
(setq w32-lwindow-modifier 'super)

(setq w32-pass-rwindow-to-system nil)
(setq w32-rwindow-modifier 'alt)

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
  (let ((v (alist-r-get default-frame-alist 'height)))
    (cond
     ((eq arg 4) (setq v (/ v 2)))
     ((eq arg 16) (setq v (/ v 4)))
     )
    (set-frame-height (selected-frame) v)
    )
  )

(defun frame-default-width (&optional arg) (interactive "p")
  (let ((v (or (alist-r-get default-frame-alist 'width) 80)))
    (cond
     ((< arg 4) (setq v (* v arg)))
     ((eq arg 16) (setq v (/ v 2)))
     )
    (set-frame-width (selected-frame) v)
    )
  )

(defun frame-width-ediff (&optional arg) (interactive "p")
  (frame-default-width 3)
  (let ((x (car (frame-position (selected-frame)))))
    (cond ((< x 0)
	   (set-frame-position (selected-frame) -4000 0)
	   )
	  ((set-frame-position (selected-frame) 0 0))
	  )
    )
  )

(defun frame-width-glitch (&optional arg)
  (interactive "p")
  (let ((width (cdr (assoc 'width (frame-parameters)))))
    (set-frame-width (selected-frame) (1+ width))
    (set-frame-width (selected-frame) width)
    )
  )

;(global-unset-key (kbd "<scroll>"))
;(define-key global-map (kbd "<scroll>") 'frame-width-glitch)

(define-key z-map "\C-g" 'frame-width-glitch)
(define-key z-map "h" 'frame-default-height) ; don't use C-h
(define-key z-map "\C-w" 'frame-default-width)
(define-key z-map "\C-e" 'frame-width-ediff)

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
  (org-defkey org-mode-map (kbd "C-'") nil)
  (org-defkey org-mode-map (kbd "C-,") nil)
  )

(add-hook 'org-load-hook 'ns-org-load-hook)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar alt-buffer-eval-function nil "Alternate buffer eval function")

(make-local-variable 'alt-buffer-eval-function)

(defun alt-buffer-eval () (interactive)
 (cond (alt-buffer-eval-function (funcall alt-buffer-eval-function)))
 )
(define-key global-map [C-f9] 'alt-buffer-eval)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(recentf-mode 1)
(setq recentf-max-saved-items nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key global-map [M-f1] 'help)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun buffer-match-name (pat buf)
  (string-match pat (buffer-name buf)))

(defun buffer-match-filename (pat buf)
  (let ((name (buffer-file-name buf))) (and name (string-match pat name))))

(defun buffer-list-filter (fun)
  (delq nil
	(mapcar (lambda (buf)
		  (cond ((funcall fun buf) buf)))
		(buffer-list))))

(defun buffer-filter-by-name (bufregexp &optional not-file-name)
  (delq nil
	(mapcar (lambda (buf)
		  (when (if not-file-name
			    (string-match bufregexp
					  (buffer-name buf))
			  (and (buffer-file-name buf)
			       (string-match bufregexp
					     (buffer-file-name buf))))
		    buf))
		(buffer-list))))

;;(buffer-list-filter '(lambda (buf) (buffer-match-filename "\\.c" buf)))

(defun buffer-list-c-files () (buffer-filter-by-name "\\.c"))

;; (buffer-list-c-files)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cygwin-version ()
  (string-match-string "cygwin\\([0-9]+\\)" (getenv "PATH") 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key global-map (kbd "M-j") bookmark-map)

(define-key bookmark-map (kbd "M-SPC") 'bookmark-bmenu-list)
(define-key bookmark-map (kbd "M-j") 'bookmark-jump)

(define-key global-map [C-f1] 'org-capture)
(define-key global-map [C-f2] 'bookmark-bmenu-list)

(define-key global-map [M-f1] 'org-cycle-agenda-files)
(define-key global-map [M-f2]
  '(lambda () (interactive)
     (find-file org-default-notes-file)
     )
  )
(global-set-key [M-f3] 'org-agenda)

(define-key global-map [M-up] 'bookmark-jump)
(define-key global-map [M-down] 'ace)

(global-set-key (kbd "C-c a") 'org-agenda)

(define-key global-map "\C-h" 'backward-char)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq diredp-hide-details-initially-flag nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defin 'with-output-file)

(setq blink-cursor-delay 1.0)
(setq blink-cursor-blinks 5)
(setq blink-cursor-interval 0.5)
;(setq blink-cursor-interval 1.0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq time-base32-hist nil)

(defun time-base32 (&optional time)
  (let ((n (floor (float-time time)))
	(s)
	)
    (while (> n 0)
      (setq i (% n 32))
      (setq s (concat (substring "abcdefghijklmnopqrstuvwxyz234567" i (1+ i)) s))
      (setq n (lsh n -5))
      )
;    (substring s 1)
    (push s time-base32-hist)
    s		; gives characteristic 'bp7' prefix
    )
  )



(defun tsx-clip () (interactive)
  (call-shell "wperl e:/ahk/clip.pl tsx")
  (clipboard-yank)
  )

(defun tsx-clip (&optional kill) (interactive)
  (setq log-time-string (time-base32))
  (insert log-time-string)
  (and kill (kill-new log-time-string))
  )

(define-key global-map [C-f7] 'tsx-clip)
;time-base32-hist

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun url-escape (x)
  (setq x (replace-regexp-in-string " " "+" x))
  )

(defun google (q &optional lucky)
  (interactive "sGoogle: \nP")
  (let* ((q (url-escape q))
	 (url (concat
	       "https://www.google.com/search?hl=en&nfpr=1&q="
	       q
	       (cond
		(lucky "&btnI=I%27m+Feeling+Lucky")
		("&btnG=Google")
		)
	      )))
;    (debug)
    (browse-url url)
    )
  )
       
(define-key global-map [f5] 'google)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun line-re (s) (concat "^.*" s ".*$"))

(defun var-re (s) (let ((ss "\\(\\sw\\|\\s_\\)*")) (concat ss s ss)))
(defun var-re-begin (s) (let ((ss "\\(\\sw\\|\\s_\\)*")) (concat s ss)))

(defun font-lock-quick (list)
  (setq font-lock-keywords list)
  (font-lock-mode t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'bg-hydra "bg-hydra" "" (interactive))
