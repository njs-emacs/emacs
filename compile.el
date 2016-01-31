(load-standard "compile")

(autoload 'append-log-entry "log")

(defun compile-log () (save-excursion (append-log-entry)))

;(setq compilation-mode-hook '(compile-log))
(setq compilation-mode-hook nil)

(setq compile-save-modes
  '(p-mode c-mode c++-mode yacc-mode lex-mode makefile-mode makefile-gmake-mode  rc-mode
	   perl-mode
	   java-mode
	   ))

(defun compile-save-some-buffers ()
  (mapcar '(lambda (x)
	     (cond ((buffer-file-name x)
		   (save-excursion
		     (set-buffer x)
		     (and (memq major-mode compile-save-modes)
			  (buffer-modified-p (current-buffer))
			  (save-buffer)))
		   )))
	  (buffer-list)))

(defun compilation-restart-errors () (interactive)
  (compilation-forget-errors)
  (setq compilation-parsing-end 1)
  )

(setq c-map (make-sparse-keymap))
(define-key esc-map "c" c-map)

(let ((map c-map))
  (define-key map "g" 'grep)
  (define-key map "x" 'xgrep)
  (define-key map "0" '(lambda () (interactive)
			 (bake-target "clean")
			 ))
  (define-key map "" 'compilation-restart-errors)
  )

(defun wcompile (cmd &optional name mode)
  (let ((buffer (get-buffer-create (or name "*compilation*"))))
    (compile cmd mode)
    (while (get-buffer-process compilation-last-buffer)
      (message "waiting")
      (sit-for 1)
      )
    )
  )

(defmacro wgrep (pat &optional spec &rest body)
  `(progn
     (wcompile
      (format "%s '%s' %s /dev/null" grep-cmd ,pat (or ,spec (grep-spec))))
     ,@body)
  )

(defun kill-compilation-buffers () (interactive)
  (mapcar '(lambda (x)
	     (set-buffer x)
	     (cond
	      ((eq major-mode 'compilation-mode) (kill-buffer x))
	      ((eq major-mode 'grep-mode) (kill-buffer x))
	      ((eq major-mode 'nog-mode) (kill-buffer x))
	      ))
	  (buffer-list))
  (setq compilation-in-progress nil)
  )

(define-key c-map "d" 'kill-compilation-buffers)

(setq compilation-wait nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(setq compilation-buffer-name-function---obsolete
;  (function (lambda (mode) 
;	      (let* ((list (subset-if '(lambda (x)
;					 (set-buffer x)
;					 (eq major-mode 'compilation-mode))
;				      (buffer-list)))
;		     )
;		(format "*%s<%s>*" mode (length list)))))
;  )

(setq compilation-buffer-name-function
  (function (lambda (mode)
	      (let ((i 0) name)
		(while
		    (progn
		      (setq name (format "*%s<%s>*" mode i))
		      (get-buffer name)
		      ) (setq i (1+ i)))
		name)
	      )
	    ))

;;(defun compilation-buffer-name (mode) (funcall compilation-buffer-name-function mode))

;;

(defun compile-21 (command)
  "Compile the program including the current buffer.  Default: run `make'.
Runs COMMAND, a shell command, in a separate process asynchronously
with output going to the buffer `*compilation*'.

You can then use the command \\[next-error] to find the next error message
and move to the source code that caused it.

To run more than one compilation at once, start one and rename the
\`*compilation*' buffer to some other name with \\[rename-buffer].
Then start the next one.

The name used for the buffer is actually whatever is returned by
the function in `compilation-buffer-name-function', so you can set that
to a function that generates a unique name."
  (interactive
   (if compilation-read-command
       (list (read-from-minibuffer "Compile command: "
                                 compile-command nil nil
                                 '(compile-history . 1)))
     (list compile-command)))
  (setq compile-command command)
  (compile-internal compile-command "No more errors"))

(defun grep+ (command-args)
  (grep command-args)
  )
  
;;;
;;;

;(load "cm")

;(add-hook 'compilation-mode-hook '(lambda () (setq truncate-lines t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar grep-pattern-hist nil)
(defvar grep-spec-hist nil)

(make-variable-buffer-local 'compile-protect)
(setq-default compile-protect nil)

(make-variable-buffer-local 'grep-path)
(setq-default grep-path ".")

(make-variable-buffer-local 'grep-flags)
(setq-default grep-flags "-n")

(make-variable-buffer-local 'grep-cmd)
(setq-default grep-cmd "grep -n")

(make-variable-buffer-local 'grep-spec)
(setq-default grep-spec nil)

(setq grep-spec-alist
  '((c-mode . "*.[chyn]")
    (p-mode . "*.p")
    (emacs-lisp-mode . "*.[enp]l")
    (c++-mode . "*.[chy] *.[ch]pp")
    (dired-mode . "*.[chyn]")
    )
  )

(defun grep-spec (&optional mode)
  (or
   (and mode (cdr (assoc mode grep-spec-alist)))
   grep-spec
   (cdr (assoc major-mode grep-spec-alist))
   (read-from-minibuffer "spec: " "*")
;   (string-substitute (call-shell "file * | grep text | cut -f 1 -d :")
;			     ?\n ? )
   )
  )

(fset 'grep-hack 'compile)

(defun ngrep (pat &optional spec flags) (interactive "sPattern: ")
  (setq spec (or spec (grep-spec)))
  (cond ((listp spec) (setq spec (cat spec " "))))
  (let ((cmd (format "grep %s \"%s\" %s" 
		     (or flags grep-flags)
		     pat
		     spec
		     ))
	(mode 'grep-mode)
	)
    (setq grep-history (cons cmd grep-history))
    (cond 
     (compilation-wait (wcompile cmd mode))
     ((compile cmd mode))
     )
    (save-excursion (set-buffer compilation-last-buffer)
;					(setq truncate-lines t)
					)
    )
  )

(defun ngrep-last-search () (interactive)
  (ngrep (car regexp-search-ring))
  )

(define-key global-map "\M-c\M-p" 'ngrep-last-search)

(defvar rgrep-spec-hist nil)

(defvar rgrep-path-default ".")
(defvar rgrep-find-spec-default nil)

(defvar rgrep-path ".")
(defvar rgrep-find-spec nil)

(defun kgrep (arg) (interactive "P")
  (ngrep (nth (or arg 0) kill-ring))
  )

(defun egrep (p &optional spec)
  (compile (format "egrep -n '%s' %s /dev/null" p (or spec (grep-spec))))
  )

(defun rgrep (p &optional spec flags path)
  (interactive 
    (list (read-from-minibuffer "Pattern: " nil
				nil nil '(grep-pattern-hist . 0))
	  (read-from-minibuffer "Spec: " (format "-name \\%s" (grep-spec))
				nil nil '(grep-spec-hist . 0))
	  ))
  (grep (format "egrep %s -e \"%s\" `find %s -type f %s -print 2>/dev/null`"
		(or flags grep-flags) 
		p
		(or path rgrep-path rgrep-path-default)
		(or spec rgrep-find-spec rgrep-find-spec-default 
		    (format "-name \\%s" (grep-spec))))
	)
  )

(define-key global-map "\M-c\M-r" 'rgrep)
(define-key global-map "\M-c\M-s" 'sgrep)
(define-key global-map "\M-c\M-k" 'kgrep)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun compile-visit (spec) (interactive "SSpec: ")
  (let* ((list (ls spec))
	 (dir default-directory)
	 (text (apply 'concat
		      (mapcar '(lambda (x) (format "%s:1: ...\n" x)) list)))
	 (buffer (get-buffer-create (compilation-buffer-name "visit" nil nil)))
	 )
    (switch-to-buffer-other-window buffer)
    (insert (format "cd %s\n...\n" dir))
    (insert text)
    (setq compilation-last-buffer buffer)
    (compilation-mode)
    (setq default-directory dir)
    (setq compilation-directory-stack (list default-directory))
    )
  )

(define-key compilation-mode-map "\C-ct"
  '(lambda () (interactive) (setq truncate-lines (not truncate-lines))
     (recenter)
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq compile-stack nil)

(defun compile-push ()
  (set-buffer "*compilation*")
  (setq compile-stack (cons (buffer-string) compile-stack))
  )

(defun compile-pop ()
  (set-buffer "*compilation*")
  (erase-buffer)
  (insert (car compile-stack))
  (setq compile-stack (cdr compile-stack))
  )

(defun compile-filter-errors (fun)
  (swx (switch-to-buffer "*compilation*")    
       (goto-line 3)
       (while (rsf "[^ :\n]+:" nil t)
	 (bol)
	 (cond ((funcall fun) (kill-line 1))
	       ((fl 1)))
	 )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq compilation-mode-special-hooks nil)

(defun compilation-mode-hook-ns ()
  (setq truncate-lines t)
  (run-hook-with-args 'compilation-mode-special-hooks)
  )

(or (assoc 'compilation-mode-hook-ns compilation-mode-hook)
    (setq compilation-mode-hook 'compilation-mode-hook-ns))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "bake" t t)
(load-overrides "compile")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq compilation-error-regexp-alist-alist
  (alist-put compilation-error-regexp-alist-alist 'gnu
	'(
     "^\\(?:[[:alpha:]_][-[:alnum:]._]+: ?\\)?\
\\([/._]*[a-zA-Z0-9]:?[^ \t\n:]*\\|{standard input}\\): ?\
\\([0-9]+\\)\\([.:]?\\)\\([0-9]+\\)?\
\\(?:-\\(?:\\([0-9]+\\)\\3\\)?\\.?\\([0-9]+\\)?\\)?:\
\\(?: *\\(\\(?:Future\\|Runtime\\)?[Ww]arning\\|W:\\)\\|\
 *\\([Ii]nfo\\(?:\\>\\|rmationa?l?\\)\\|I:\\|instantiated from\\)\\)?"
     1 (2 . 5) (4 . 6) (7 . 8))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; add split to compilation regexps

(setq compilation-error-regexp-alist-alist
  (alist-put compilation-error-regexp-alist-alist 'splint
	'(
     "^\\(.*?\\)(\\([0-9]+\\),\\([0-9]+\\))"
     1 2 3)))


(setq compilation-error-regexp-alist
 (mapcar 'car compilation-error-regexp-alist-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cygdrive-kill ()
  (setq string (replace-regexp-in-string "/cygdrive/\\(.\\)/" "\\1:/" string))
  )

(add-hook 'compilation-filter-pre-insert-hook 'cygdrive-kill)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun compilation-canonify-filename (filename)
  (cond
   ((string-match "^/cygdrive/\\(\\sw\\)/\\(.*\\)" filename)
    (concat (match-string 1 filename) ":/" (match-string 2 filename))
    )
   (filename)
   )
  )

(defun compilation-find-file (marker filename directory &rest formats)
  "Find a buffer for file FILENAME.
Search the directories in `compilation-search-path'.
A nil in `compilation-search-path' means to try the
\"current\" directory, which is passed in DIRECTORY.
If DIRECTORY. is relative, it is combined with `default-directory'.
If DIRECTORY. is nil, that means use `default-directory'.
If FILENAME is not found at all, ask the user where to find it.
Pop up the buffer containing MARKER and scroll to MARKER if we ask the user."
  (setq filename (compilation-canonify-filename filename))
  (or formats (setq formats '("%s")))
  (let ((dirs compilation-search-path)
        (spec-dir (if directory
                      (expand-file-name directory)
                    default-directory))
        buffer thisdir fmts name)
    (if (file-name-absolute-p filename)
        ;; The file name is absolute.  Use its explicit directory as
        ;; the first in the search path, and strip it from FILENAME.
        (setq filename (abbreviate-file-name (expand-file-name filename))
              dirs (cons (file-name-directory filename) dirs)
              filename (file-name-nondirectory filename)))
    ;; Now search the path.
    (while (and dirs (null buffer))
      (setq thisdir (or (car dirs) spec-dir)
            fmts formats)
      ;; For each directory, try each format string.
      (while (and fmts (null buffer))
        (setq name (expand-file-name (format (car fmts) filename) thisdir)
              buffer (and (file-exists-p name)
                          (find-file-noselect name))
              fmts (cdr fmts)))
      (setq dirs (cdr dirs)))
    (while (null buffer)    ;Repeat until the user selects an existing file.
      ;; The file doesn't exist.  Ask the user where to find it.
      (save-excursion            ;This save-excursion is probably not right.
        (let ((pop-up-windows t))
          (compilation-set-window (display-buffer (marker-buffer marker))
                                  marker)
          (let* ((name (read-file-name
                        (format "Find this %s in (default %s): "
                                compilation-error filename)
                        spec-dir filename t nil))
                 (origname name))
            (cond
             ((not (file-exists-p name))
              (message "Cannot find file `%s'" name)
              (ding) (sit-for 2))
             ((and (file-directory-p name)
                   (not (file-exists-p
                         (setq name (expand-file-name filename name)))))
              (message "No `%s' in directory %s" filename origname)
              (ding) (sit-for 2))
             (t
              (setq buffer (find-file-noselect name))))))))
    ;; Make intangible overlays tangible.
    ;; This is weird: it's not even clear which is the current buffer,
    ;; so the code below can't be expected to DTRT here.  -- Stef
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'intangible)
        (overlay-put ov 'intangible nil)))
    buffer))

