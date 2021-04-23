;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; go-boo is an interesting code generation technique
;; where code is piped out to a perl script which replaces
;; the code with a transformed version according to ad-hoc
;; pseudo-language forms
;; the original input from the target buffer may be discarded
;; or modified or reflected back intact. That depends on the
;; boobie.
;; All the code is in the boo script, special boobies may
;; be present which offer standard services
;;
;; bum - execute code for side-effects only

(defvar go-boo-script "boo.pl" "The script file to execute for boo commands")
(make-local-variable 'go-boo-script)

(defvar go-boo-prev "main" "The previous boobie")
(defvar boo-history nil "The history of boobies entered interactively")

(defvar-local go-boo-env nil "Extra buffer-local arg for boo.pl")

(defvar go-boo-script-paths
  `(
    "boo.pl"
    ".boo/boo.pl"
    "boo/boo.pl"
    )
  )

(defun boo-get-script-or-die ()
  "Find the boo script (for example boo.pl) somewhere in the current
path or ancestors of the current path.
Throw an error if script cannot be found."
  (let* ((script
	  (or (locate-up-file go-boo-script)
	      (car (member-if '(lambda (x) (locate-up-file x)) go-boo-script-paths)))
	  ))
    (cond (script (expand-file-name script))
	  ((error "no script found"))
	  )
    )
  )

(defun boo-command-build ()
  "Build the boo.pl command line, using higher scope arguments."
  (let (cmd (loop (cond (loop 1) (0))))
    (setq cmd
      (format "perl %s --fun=%s --loop=%s --mode=%s --file=\"%s\""
	      script fun loop major-mode (buffer-file-name))
      )
    (cond
     (go-boo-env
      (setq cmd (concat cmd (format " --env=\"%s\"" go-boo-env)))
      )
     )
    cmd
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun boo-raw (fun)
  "Execute boobie FUN purely for side effects, with no input.
Output is handled the same as shell-command-on-region."
  (interactive (list (read-string "Fun: " (car boo-history) 'boo-history)))
		     
  (cond ((equal fun "") (setq fun go-boo-prev)))
  (setq go-boo-prev fun)

  (let* ((script (boo-get-script-or-die))
	 (loop nil)
	 )
    (setq cmd (boo-command-build))
    (shell-command-on-region (point) (point) cmd nil)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun boo-boo (fun &optional loop)
  "Execute boobie FUN on region (with optional LOOP) and replace
 the region with the output."
  (interactive 
    (list (read-string "Fun: " (car boo-history) 'boo-history)
	  current-prefix-arg))
  (cond ((equal fun "") (setq fun go-boo-prev)))
  (setq go-boo-prev fun)
  (let* ((script (boo-get-script-or-die))
	 start end cmd
	 )

    (cond
     ((region-active-p)
      (setq start (region-beginning) end (region-end))
      )
     ((looking-at "^$")
      (setq start (point) end (point))
      )
     ((bolp)
      (setq start (point) end (point$))
      )
     (t
      (setq start (point) end (point$))
      )
     )
    (copy-region-as-kill start end)

    (setq cmd (boo-command-build))
    (shell-command-on-region start end cmd nil t)
    (bs (point) (mark))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun boo-bum ()
  "Execute (boo-boo \"bum\") on boo-region. This will normally
be used to execute boobies for side-effects only (using bum boobie)."
  (interactive)
  (boo-boo "bum")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun boo-boom ()
  "Execute (boo-boo \"boom\") on boo-region.
This is the shortcut generic content-regeneration boobie 'boom."
  (interactive)
  (boo-boo "boom")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key global-map (kbd "<s-f10>") 'boo-boo)
(define-key global-map (kbd "<s-f11>") 'boo-boom)
(define-key global-map (kbd "<s-insert>") 'boo-boom)
(define-key global-map (kbd "<s-f12>") 'boo-bum)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; non-interactive functions

(defun boo-ni (text &optional fun loop)
  "Execute boobie FUN on TEXT (with optional LOOP).
Return the output from bum boobie, and append to the \" *bum*\" buffer."
  (let* ((script (boo-get-script-or-die))	 
	 (fun (or fun (car boo-history)))
	 (file-name (buffer-file-name))
	 buffer start end cmd
	 )
    (setq cmd (boo-command-build))
    (setq buffer (get-buffer-create " *boo-ni*"))
    (sx
     (set-buffer buffer)
     (setq default-directory (file-name-directory file-name))
     (goto-char (setq start (point-max)))
     (insert text)
     (shell-command-on-region start (point-max) cmd nil t)
     (insert "\n")
     (bs start (point-max))
     )
    )
  )

(defun boo-ni-bum (text &optional loop)
  "Execute boobie 'bum' on TEXT (with optional LOOP).
Return the output from bum boobie, and append to the \" *bum*\" buffer."
  (boo-ni text "bum" loop)
  )

