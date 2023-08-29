(defun kill-new-verbose (s)
  (kill-new s)
  (message "Copied to kill: '%s'" s)
  )

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
     (eq major-mode 'js-mode)
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

(def-key-global (kbd "C-=") 'comment-line-insert)

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
  (kill-new-verbose
   (cond
    ((= arg 1) (file-name-nondirectory (buffer-file-name)))
    ((= arg 0) (basename (buffer-file-name)))
    ((= arg 2) (file-name-extension (buffer-file-name)))
    ((= arg 4) (buffer-file-name))
    ((= arg 16) (format "%s:%s" (buffer-file-name) (line-number-at-pos (point))))
    (t (format "arg: %s" arg))
    )
   )
  )

(define-key global-map (kbd "C-z C-y") 'filename-to-kill)
(def-key z-map (kbd "C-f") 'filename-to-kill)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dired-quick-dot (arg) (interactive "p")
  (dired default-directory)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun toggle-tab-width () (interactive) "Toggle tab width between 4 and 8"
  (setq tab-width (if (eq tab-width 8) 4 8))
  (redraw-display)
  )

(def-key-global (kbd "M-u TAB") 'toggle-tab-width)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(def-key-global (kbd "M-_") 'kill-paren)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cfind (arg &optional path)
  (compile
   (format "find %s %s 2>/dev/null | sed 's/$/:1:/'" (or path ".") arg))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq completion-rank
  '(
    (".*\\.KTM$" nil (".*\\.P$"))
    )
  )

(defun member-string-match (key list)
  (member-if '(lambda (pat) (string-match pat key)) list)
  )

(defun assoc-string-match (key list)
  "Return member of LIST which has a car which string-matches KEY"
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

(define-key minibuffer-local-completion-map [select] 'minibuffer-complete-preferred)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun show-result (arg) (interactive "P")
  (let ((fun (cond (arg (intern (format "show-%s" arg)))
		   ('prin))))
    (show (funcall fun eval-result)))
  )

(def-key-global (kbd "M-u M-u") 'show-result)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rename-buffer-as-backup (&optional buffer) (interactive)
  (let* ((old (buffer-object buffer)))
    (cond (old
	   (setq new (buffer-name (generate-new-buffer (buffer-name old))))
	   (kill-buffer new)
	   (save-excursion (set-buffer old) (rename-buffer new))
	   ))))

(def-key-global (kbd "M-f M-b") 'rename-buffer-as-backup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(define-key minibuffer-local-completion-map "\M- "  'preferred-completion)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this was a promising idea for very quick emacs-lisp snippets
;; but needs a rewrite

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun insert-time-string () (interactive)
  (insert (format-time-string "%y%m%d%H%M%S " (current-time)))
  )

(defun truncate-lines-toggle () (interactive)
  (setq truncate-lines (not truncate-lines))
  (recenter)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (define-key global-map [mode-line drag-mouse-1] 'mldrag-drag-mode-line)

;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; take over help key

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; not sure i like this
(cond ((boundp 'want-icicle)
       (let ((load-path (cons (format "%s/0812/icicles" user-emacs-home) load-path)))
	 (load-library "icicles")
	 )
       )
      )
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
  (kill-new-verbose (format "emacs:%s??%s" (buffer-file-name) (line-number-at-pos)))
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
    (kill-new-verbose (format "[[%s][%s]]" link cap))
    (fset 'zz `(lambda () (interactive)
		 (insert ,(format "[[%s][" link))
		 (sx (insert ,cap "]"))
		 )
	  )
    )
  )

(def-key z-map (kbd "C-c") 'copy-emacs-url)
(def-key z-map (kbd "C-o") 'copy-org-link)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun copy-gdb-break (&optional arg)
  (interactive "P")
  
  (let* ((file (file-name-nondirectory (buffer-file-name)))
	 (cap (or (region-text) (format "%d" (line-number-at-pos))))
	 (break (format "break %s:%s" file cap))
	 )
    (kill-new-verbose break)
    )
  )
(def-key z-map (kbd "C-v") 'copy-gdb-break)

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
(defun frame-width-set-monitor-aware (new-width &optional frame)
  (let* ((frame (or frame (selected-frame)))
	 (geometry (frame-monitor-geometry frame))
	 (old-width (frame-width))
	 (display-x (nth 0 geometry))
	 (display-width (nth 2 geometry))
	 (frame-pos (frame-position frame))
	 (frame-x (car frame-pos))
	 (frame-pixel-width (frame-native-width frame))
	 (char-width (frame-char-width frame))
	 (fudge 8)
	 (spill
	  (- (+
	      fudge frame-x frame-pixel-width
	      (* (- new-width old-width) char-width)
	      )
	     display-width
	     )
	  )
	 )
    (cond ((> spill 0)
	   (set-frame-position frame (- frame-x spill) (cdr frame-pos))
	   )
	  )
    (set-frame-width (selected-frame) new-width)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun frame-default-width-get ()
  (or (alist-r-get default-frame-alist 'width) 80))

(defun frame-default-height-get ()
  (or (alist-r-get default-frame-alist 'height) 100))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun frame-default-height (&optional arg) (interactive "p")
  (let ((v (frame-default-height-get)))
    (cond
     ((eq arg 4) (setq v (/ v 2)))
     ((eq arg 16) (setq v (/ v 4)))
     )
    (set-frame-height (selected-frame) v)
    )
  )

(defun frame-default-width (&optional arg) (interactive "p")
  (let ((v (frame-default-width-get)))
    (cond
     ((< arg 4) (setq v (* v arg)))
     ((eq arg 16) (setq v (/ v 2)))
     )
    (frame-width-set-monitor-aware v)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun frame-width-ediff (&optional arg)
  (interactive "p")
  (let* ((width (frame-width))
	 (count (cond ((or ediff-3way-comparison-job ediff-merge-job) 3) (2)))
	 (max-width (* (frame-default-width-get) count))
	 )
    (cond
     ((< width max-width)
      (frame-width-set-monitor-aware max-width (selected-frame))
      )
     (t (set-frame-width (selected-frame) (frame-default-width-get)))
     )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun frame-width-glitch (&optional arg)
  (interactive "p")
  (let ((width (cdr (assoc 'width (frame-parameters)))))
    (set-frame-width (selected-frame) (1+ width))
    (set-frame-width (selected-frame) width)
    )
  )

;(global-unset-key (kbd "<scroll>"))
;(define-key global-map (kbd "<scroll>") 'frame-width-glitch)

(def-key z-map "\C-g" 'frame-width-glitch)
(def-key z-map "h" 'frame-default-height) ; don't use C-h
(def-key z-map "\C-w" 'frame-default-width)
(def-key z-map "\C-e" 'frame-width-ediff)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(defun rs () (interactive) (insert (prin (read-key-sequence "???"))))

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

(def-key-global [M-f7] 'insert-file-timestamp)

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
		  "C:/Program Files/SmartGit/bin/smartgit.exe"
		  "--open"
		  d))
      (set-process-sentinel proc 'smart-git-process-sentinel)
     )
    )
    )
  )

(def-key-global [M-f11] 'smart-git-here)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(highlight-lines-matching-regexp "open" nil)
;(apropos "process")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar alt-buffer-eval-function nil "Alternate buffer eval function")

(make-local-variable 'alt-buffer-eval-function)

(defun alt-buffer-eval () (interactive)
 (cond (alt-buffer-eval-function (funcall alt-buffer-eval-function)))
 )
(def-key-global [C-f9] 'alt-buffer-eval)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(recentf-mode 1)
(setq recentf-max-saved-items nil)

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
  (and kill (kill-new-verbose log-time-string))
  )

(def-key-global [C-f7] 'tsx-clip)

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
       
(def-key-global [f5] 'google)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun generic-arg-alist-find (args-pattern)
  "Find an alist which follows a pattern match on the current line,
 or the first match in the buffer. Used to override default compilation or buffer evaluation functions."

  (sx
   (bol)
   (cond 
    ((rsf args-pattern) (eval (readc)))
    ((bob)
     (cond
      ((rsf args-pattern) (eval (readc)))
      )
     )
    )
  )
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-buffer-string (buffer &optional start end)
  (sx (set-buffer buffer)
      (buffer-substring-no-properties (or start (point-min))
				      (or end (point-max)))
      )
  )

; (get-buffer-string "*Shell Command Output*")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun def-key-buffer (key form &optional map)
  (define-key (or map global-map) key
    `(lambda () (interactive) (switch-to-buffer (eval ,form)))
    )
  )

(def-key-buffer (kps "11") 'org-agenda-buffer)

(define-key global-map (kps "^" "`") 'help)

(def-key-buffer (kps "^1") 'org-agenda-buffer)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun beginning-of-defun-safe ()
  "Goto the beginning of the current defun. Try to not move if already at the beginning.
 The idea is to not move to the previous defun, stay in the current one."
  (interactive)
  (rsf "\\S ")
  (beginning-of-defun)
  )
    
(defun end-of-defun-safe ()
  "Goto the end of the current defun. Try to not move if already at the end.
 The idea is to not move to the next defun, stay in the current one."
  (interactive)
  (rsb "\\S ")
  (end-of-defun)
  )
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq region-dwim-default-type 'line)
; (setq region-dwim-default-type 'buffer)
; (setq region-dwim-default-type 'defun)

(defun region-dwim-arg-to-type (&optional arg)
  "Convert an interactive prefix arg to a region-dwim type"
  (case arg
    (0 'defun)
    (4 'line)
    (t region-dwim-default-type)
    )
  )

(defun region-dwim-beginning (&optional type)
  "Return the beginning of an implied region."
  (or (region-beginning-if-active)
      (case (or type region-dwim-default-type)
	(defun (sxp (beginning-of-defun-safe)))
	(line (point^))
	(buffer (point-min))
	(t (point-min))
	)
      )
  )

(defun region-dwim-end (&optional type)
  "Return the end of an implied region."
  (or (region-end-if-active)
      (case (or type region-dwim-default-type)
	(defun (sxp (end-of-defun-safe)))
	(line (point$))
	(buffer (point-max))
	(t (point-max))
	)
      )
  )

(defun region-dwim (&optional type)
  "Return the boundaries of the implied region"
  (list
   (region-dwim-beginning type)
   (region-dwim-end type)
   )
  )

(defun region-dwim-text (&optional type)
  "Return the text in the implied region"
  (buffer-substring
   (region-dwim-beginning type)
   (region-dwim-end type)
   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun edit-value (name &rest plist)
  "Edit a variable value."
  (interactive)
  (let* ((buf (get-buffer-create (format "*edit-%s*" name)))
	 (value (eval name))
	 (eformat (or (plist-get plist :eformat)
		      '(lambda (x) (format "%S" x))
		      ))
	 )
    (switch-to-buffer buf)
    (erase-buffer)
    (insert ";;; -*- mode: emacs-lisp -*-
;;;
;;; f9		- eval this buffer

")

    (insert (format "(setq %s\n `(\n" name))
    (dolist (i value)
      (insert "   " (funcall eformat i) "\n")
      )	      
    (insert "  ))\n")
    )
  (insert (format"\n\n%s\n\n" (or (plist-get plist :tail) "")))
  (emacs-lisp-mode)
  )

;; (edit-value 'org-refile-targets)
;; (edit-value 'ziip :tail ";yo")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun apply-to-car-pair (fun a b) (funcall fun (car a) (car b)))
(defmacro def-apply-to-car-pair (fun)
 `(defun ,(intern (format "%s-car" fun)) (a b) (apply-to-car-pair ',fun a b))
 )

(def-apply-to-car-pair string<)
(def-apply-to-car-pair string=)
(def-apply-to-car-pair string>)

(def-apply-to-car-pair max)
(def-apply-to-car-pair min)

(def-apply-to-car-pair >)
(def-apply-to-car-pair <)
(def-apply-to-car-pair >=)
(def-apply-to-car-pair <=)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun apply-to-cdr-pair (fun a b) (funcall fun (cdr a) (cdr b)))

(defun max-cdr (a b) (apply-to-cdr-pair 'max a b))
(defun min-cdr (a b) (apply-to-cdr-pair 'min a b))
(defun gt-cdr (a b) (apply-to-cdr-pair '> a b))
(defun lt-cdr (a b) (apply-to-cdr-pair '< a b))
(defun ge-cdr (a b) (apply-to-cdr-pair '>= a b))
(defun le-cdr (a b) (apply-to-cdr-pair '<= a b))

(defun string<-cdr (a b) (apply-to-cdr-pair 'string< a b))
(defun string>-cdr (a b) (apply-to-cdr-pair 'string> a b))
(defun string=-cdr (a b) (apply-to-cdr-pair 'string= a b))

(defmacro def-apply-to-cdr-pair (fun)
 `(defun ,(intern (format "%s-cdr" fun)) (a b) (apply-to-cdr-pair ',fun a b))
 )

; (macroexpand '(def-apply-to-cdr-pair string-lessp))
(def-apply-to-cdr-pair string-lessp)
(def-apply-to-cdr-pair string-greaterp)

(def-apply-to-cdr-pair equal)
(def-apply-to-cdr-pair eq)

;;; (max-cdr '(a . 8) '(c . 3))
;;; (min-cdr '(a . 8) '(c . 3))
;;; (gt-cdr '(a . 8) '(c . 3))
;;; (lt-cdr '(a . 8) '(c . 3))
;;; (ge-cdr '(a . 8) '(c . 3))
;;; (le-cdr '(a . 8) '(c . 3))
;;; (le-cdr '(a . 3) '(c . 3))
;;; 
;;; (string<-cdr '(a . "3") '(c . "4"))
;;; (string-lessp-cdr '(a . "3") '(c . "4"))
;;; (string>-cdr '(a . "3") '(c . "4"))
;;; 
;;; (equal-cdr '(a . "4") '(c . "4"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun copy-dup-list (list)
  "Make a copy of a LIST appended to itself. This is used to move along
a cyclic list in either direction."
 (nconc (copy-list list) (copy-list list)))

(defun list-next-list (list elt &optional fun)
  "Return a LIST which has at its head the (cyclic) cell which follows the ELEMENT.
Use FUN (default 'equal) for the comparison."
  (let* ((fun (or fun 'equal))
	 (list (copy-dup-list list))
	 (fm (member-if `(lambda (x) (,fun x elt)) list))
	 )
    fm
    ))

(defun list-next (list elt &optional fun)
  "Return the element which follows (cyclically) ELT in LIST. Use FUN for equality check (default 'equal)."
  (cadr (list-next-list list elt fun))
  )

(defun list-prev (list elt &optional fun)
  "Return the element which precedes (cyclically) ELT in LIST. Use FUN for equality check (default 'equal)."
  (list-next (reverse list) elt fun)
  )

;; (list-next '(1 2 3 4 5) 3)
;; (list-prev '(1 2 3 4 5) 1)
;; (list-next-list '(1 2 3 4 5) 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun file-name-suffix-match (pat name)
  (string= (file-name-suffix name) pat)
  )

(defun file-name-suffix-rename (name suffix)
  (concat (file-name-base name) suffix)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; added a separator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun query-replace-dwim (from to)
  (interactive (list (read-from-minibuffer "Replace: " (thing-at-point 'symbol))
		     (read-from-minibuffer "With: " (thing-at-point 'symbol))
		     ))
;; added this line instead
  (query-replace from to)
  )

(def-key global-map (kbd "M-g %") 'query-replace-dwim)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun drorg (pat &optional since &rest args)
  (apply 'dregf pat "\\.org" (or since "ever") args)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar admin-user-init nil "Admin user passed from command line")
(defvar admin-user nil "Admin user")

(defun frame-background-color-override (color)
  (interactive "sFrame background color: ")
  (setq default-frame-alist (alist-put default-frame-alist 'background-color color))
  (mapcar '(lambda (frame) (modify-frame-parameters frame `((background-color . ,color)))) (frame-list))
  )

(defun admin-user-reflect (mode old new)
  (cond
   ((eq mode t)		; new value set
    (cond
     ((eq arg nil)
      (setq frame-title-format `("%b"))
      (frame-background-color-override "LightGray")
      )
     (t
      (setq frame-title-format `("Admin - %b"))
      (frame-background-color-override "#c6c8e0")
      )
    ))
   (t			; old value unset
    )
   )
  )

(defun set-admin-user (&optional arg)
  (interactive "p")
  (let ((old admin-user))
    (cond
     ((eq arg 0) (setq arg nil))
     ((eq arg 1) (setq arg (not admin-user)))
     ((eq arg 4) (setq arg t))
     )
    (admin-user-reflect nil admin-user arg)
    (setq admin-user arg)
    (admin-user-reflect t old arg)
    )
  )

(defun set-init-admin-user ()
  (set-admin-user admin-user-init)
  (remove-hook 'window-setup-hook 'set-init-admin-user)
  )

(add-hook 'window-setup-hook 'set-init-admin-user)

(defun shell-rb (command &optional sp ep)
  (let ((sp (or sp "^=\\s("))
	(ep (or ep "^=\\s)"))
	)
    (shell-command-on-region
     (ssb sp nil nil nil t)
     (ssb ep)
     command)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bit-set-p (v mask &optional match)
  "Return non-nil if a bit pattern is matched in VALUE."
  (= (logand v mask) (or match mask))
  )
