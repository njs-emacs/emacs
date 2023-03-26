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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; the boo.pl script expects the code to be in the input stream
;; so if we want to invoke a generator directly without using a boobie
;; (which expects an input stream)
;;
;; boo-ni-bosr and boo-ni-bofr can synthesize an input stream from
;; parameter text
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; These methods are just an alternative to using emacs lisp
;; to generate content. They can utilize the more expressive 
;; power of the perl language.
;;
;; They should be allowed to mature
;;
;; They also should go through a systematic renaming away from the
;; term 'boo'
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar go-boo-script "boo.pl" "The script file to execute for boo commands")
(make-local-variable 'go-boo-script)

(defvar go-boo-prev "main" "The previous boobie")
(defvar boo-history nil "The history of boobies entered interactively")

(defvar-local go-boo-env nil "Extra buffer-local arg for boo.pl")

(defvar go-boo-script-paths nil)
(setq go-boo-script-paths
  `(
    "boo.rb"
    "boo.pl"
    ".boo/boo.rb"
    ".boo/boo.pl"
    "boo/boo.rb"
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

(defun boo-command-build-perl (script fun loop)
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

(defun boo-command-build-ruby (script fun loop)
  "Build the boo.rb command line, using higher scope arguments."
  (let (cmd (loop (cond (loop 1) (0))))
    (setq cmd
      (format "ruby %s --fun=%s --loop=%s --mode=%s --file=\"%s\""
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

(defun boo-command-build (script fun loop)
  "Build the boo.pl command line, using higher scope arguments."
  (cond
   ((string-match "\\.rb$" script) (boo-command-build-ruby script fun loop))
   ((string-match "\\.pl$" script) (boo-command-build-perl script fun loop))
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
    (setq cmd (boo-command-build script fun loop))
    (shell-command-on-region (point) (point) cmd nil)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar-local boo-always-bol t
  "When non-nil always start expression at beginning-of-line regardless of where point is currently"
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
     (boo-always-bol
      (setq start (point^) end (point$))
      )
     (t
      (setq start (point) end (point$))
      )
     )
    (copy-region-as-kill start end)

    (setq cmd (boo-command-build script fun loop))
    (shell-command-on-region start end cmd nil t)
    (bs (point) (mark))
    )
  )

(defun boo-history-add (fun)
  (cond ((eq fun (car boo-history)))
	((setq boo-history (cons fun boo-history))))
  )

(defun boo-boo-ni (fun &optional loop)
  "Execute boobie FUN on region (with optional LOOP) and replace
 the region with the output."
  (cond ((equal fun "") (setq fun go-boo-prev)))
  (boo-history-add fun)
  (boo-boo fun loop)
  )

(defun boo-boo-again (&optional loop)
  "Execute previous boobie FUN again."
  (interactive (list current-prefix-arg))
  (boo-boo (car boo-history) loop)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun boo-boom ()
  "Execute (boo-boo \"boom\") on boo-region.
This is the shortcut generic content-regeneration boobie 'boom."
  (interactive)
  (boo-boo "boom")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bo functions - execute code for side-effects only
;; bosr - simple syntax - replace expression with result
;; bosa - simple syntax - append result to expression
;; bofr - full syntax - replace expression with result
;; bofa - full syntax - append result to expression
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun boo-bosa (&optional loop)
  "Execute (boo-boo \"bosa\") on boo-region. This will normally
be used to execute boobies for side-effects only (using bosa boobie).
Simple syntax Append result"
  (interactive "P")
  (boo-boo-ni "bosa" loop)
  )

(defun boo-bosr (&optional loop)
  "Execute (boo-boo \"bosr\") on boo-region. This will normally
be used to execute boobies for side-effects only (using bosr boobie).
Simple syntax Replace result"
  (interactive "P")
  (boo-boo-ni "bosr" loop)
  )

(defun boo-bofa (&optional loop)
  "Execute (boo-boo \"bofa\") on boo-region. This will normally
be used to execute boobies for side-effects only (using bofa boobie).
Full syntax Append result"
  (interactive "P")
  (boo-boo-ni "bofa" loop)
  )

(defun boo-bofr (&optional loop)
  "Execute (boo-boo \"bofr\") on boo-region. This will normally
be used to execute boobies for side-effects only (using bofr boobie).
Full syntax Replace result"
  (interactive "P")
  (boo-boo-ni "bofr" loop)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; non-interactive functions

(defun boo-ni (text &optional fun loop)
  "Execute boobie FUN on TEXT (with optional LOOP).
Return the output from bum boobie, and append to the \" *boo-ni*\" buffer."
  (let* ((script (boo-get-script-or-die))	 
	 (fun (or fun (car boo-history)))
	 (file-name (buffer-file-name))
	 buffer start end cmd
	 )
    (cond ((eq fun (car boo-history))) ((setq boo-history (cons fun boo-history))))
    (setq cmd (boo-command-build script fun loop))
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

(defun boo-ni-bosr (text &optional loop)
  "Execute boobie 'bosr' on TEXT (with optional LOOP).
Return the output from bosr boobie, and append to the \" *boo-ni*\" buffer."
  (boo-ni text "bosr" loop)
  )

(defun boo-ni-bofr (text &optional loop)
  "Execute boobie 'bofr' on TEXT (with optional LOOP).
Return the output from bofr boobie, and append to the \" *boo-ni*\" buffer."
  (boo-ni text "bofr" loop)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
