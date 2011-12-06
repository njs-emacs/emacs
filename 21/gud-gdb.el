(load "gud")

(defmacro gud-defq (func cmd key &optional doc)
  (list 'progn
	(list 'defun func '(arg)
	      (or doc "")
	      '(interactive "p")
	      (list 'gud-call cmd 'arg))
	(if key
	    (list 'define-key
		  '(current-local-map)
		  key
		  (list 'quote func)))
	(if key
	    (list 'global-set-key
		  (list 'concat 'gud-key-prefix key)
		  (list 'quote func)))))

(add-hook 'gud-mode-hook 'gud-local-init)

(defun gud-local-init ()
  (modify-syntax-entry ?_ "w")

;  (gud-defq gud-break  "break %f:%l"  "\C-b")
;  (gud-defq gud-tbreak "tbreak %f:%l" "\C-t")
;  (gud-defq gud-remove "clear %l"     "\C-d")
  (gud-defq gud-step   "step %p"      "\M-s")
  (gud-defq gud-stepi  "stepi %p"     "\M-i")
  (gud-defq gud-next   "next %p"      "\M-n")
  (gud-defq gud-cont   "cont"         "\M-c")
  (gud-defq gud-finish "finish"       "\M-f")
  (gud-defq gud-finish "finish"       "\M-r")
  (gud-defq gud-up     "up %p"        "\M-u")
  (gud-defq gud-down   "down %p"      "\M-d")
  (gud-defq gud-print  "print %e"     "\M-p")

  (define-key (current-local-map) "\M-a" gdb-a-map)

  (local-set-key "\M-\C-n" 'comint-next-input)
  (local-set-key "\M-\C-p" 'comint-previous-input)

;  (setq gdb-exec-dir (file-name-directory path))
;  (setq gdb-exec-file (file-name-nondirectory path))

;  (gdb-defun-key "\ea\ef" (find-file-other-window "gdb.tl"))
  )

(defmacro def-gdb (name key &optional doc)
  (let* ((fun (intern (format "gdb-%s" name)))
	 (cstr (list 'if '(not (= 1 arg))
		     (list 'format "%s %s" name 'arg)
		     name)))
    (list 'progn
 	  (list 'defun fun '(arg)
		(or doc "")
		'(interactive "p")
		(list 'gdb-call cstr))
	  (list 'define-key '(current-local-map) key  (list 'quote fun)))))

;
;
;

(defun gdb-proc ()
  (and (boundp 'gud-comint-buffer)
       (get-buffer-process gud-comint-buffer)))

(setq gdb-exec-dir nil)
(setq gdb-exec-file nil)

(defun gdb-quick (arg) (interactive "P")
  (if arg (call-interactively 'gdb)
    (if (not (and (boundp 'gud-comint-buffer)
		  gud-comint-buffer
		  (buffer-name gud-comint-buffer)))
	(if gdb-exec-file
	    (gdb (concat gdb-exec-dir gdb-exec-file))
	  (call-interactively 'gdb))))
  (switch-to-buffer gud-comint-buffer)
  (eob)
  (sit-for 5)
  )

(defun gdb-init-hook ()
  )

(setq gdb-mode-hook 'gdb-init-hook)

(defun gdb-send (&rest s) (interactive)
  (save-window-excursion
    (set-buffer gud-comint-buffer)
    (goto-char (point-max))
    (set-marker comint-last-input-start
		(process-mark (gdb-proc)))
    (insert (cat s "\n"))
    (comint-send-input)
    (end-of-buffer)
    (sit-for 1)
    ))

(defun gdb-ssend (s) (interactive)
  (process-send-string (gdb-proc) (concat s "\n"))
  )

(defun gdb-p () (interactive)  (gdb-ssend (concat "p " (x-get-cut-buffer))))
(defun gdb-p* () (interactive) (gdb-ssend (concat "p *" (x-get-cut-buffer))))

(defun gdb-p-fun (f args)
  (gdb-send (format "p %s(%s)" f args)))

(defun gdb-p-fun-x (f)
  (gdb-fun f (x-get-cut-buffer)))

(autoload 'gdb-attach-process "gdb-attach")

(setq gdb-a-map (make-sparse-keymap))

(define-key gdb-a-map "\ex" 'gdb-reset)
(define-key gdb-a-map "p" 'gdb-p)
(define-key gdb-a-map "\ep" 'gdb-p*)
(define-key gdb-a-map "\ea" 'gdb-attach-process)

(defun gdb-define (key name function)
  (define-key gdb-mode-map key
    (list 'lambda () '(interactive) (list 'gdb-send name)))
  (gdb-send (format "define %s\n%s\nend\n" name function)))

(defmacro gdb-defun-key (k &rest body)
  (list 'define-key 'gdb-mode-map k (cons 'ilambda body)))

(defmacro gdb-def (key &rest body)
  (define-key gdb-mode-map key
    (apply 'list 'lambda nil '(interactive) body))
  nil)

(setq gdb-filter-stack nil)

(defun gdb-filter-push (fun)
  (setq gdb-filter-stack (cons (process-filter (gdb-proc)) gdb-filter-stack))
  (set-process-filter (gdb-proc) fun))

(defun gdb-filter-pop ()
  (set-process-filter (gdb-proc) (car gdb-filter-stack))
  (setq gdb-filter-stack (cdr gdb-filter-stack)))

(defun gdb-get-filter (proc string)
  (setq gdb-output (concat gdb-output string))
  )

(defun gdb-get (s &optional match)
  (setq match (or match "(gdb)"))
  (let (i)
    (gdb-filter-push 'gdb-get-filter)
    (setq gdb-output "")
    (gdb-ssend s)
    (unwind-protect
	(while
	    (not (setq i (string-match match gdb-output)))
	  (accept-process-output (gdb-proc)))
      (gdb-filter-pop))
    (substring gdb-output 0 i))
  )

(setq gdb-blist nil)

(defun gdb-xbreak (name) (interactive "P")
  (setq name (or name (read-minibuffer "break: " (x-get-cut-buffer))))
  (or (memq name gdb-blist) (setq gdb-blist (cons name gdb-blist)))
  (gdb-send (format "break %s" name))
  )
(define-key gdb-a-map "\eb" 'gdb-xbreak)

(defun gdb-reload () (interactive)
  (gdb-get "reload")
  (mapcar '(lambda (x) (gdb-send (format "break %s" x))) gdb-blist)
  )
(define-key gdb-a-map "\ex" 'gdb-reload)

(defun gdb-reprint () (interactive)
  (sx (rsb (concat gdb-prompt-pattern "\\(p .*\\)") nil t))
  (insert (match-string 1)))

(define-key gdb-a-map "P" 'gdb-reprint)

(defun gdb-goto () (interactive)
  (pop-to-buffer (marker-buffer overlay-arrow-position))
  (goto-char overlay-arrow-position))

(define-key gdb-a-map " " 'gdb-goto)

(defin 'gdb-def)
;;;(gdb-def "\M-p\M-p"
;;;  (insert "set $_=pp()") (bc 1))
;;;
;;;(gdb-def "\M-p\M-x"
;;;  (gdb-send (format "set $_=pp(%s)" (x-get-cut-buffer))))
;;;
;;;(gdb-def "\M-p\M-n"
;;;  (gdb-send (format "set $_=pp(Fname(%s))" (x-get-cut-buffer))))
;;;
;;;(gdb-def "\M-p\M-o"
;;;  (gdb-send (format "set $_=pp(%s)"
;;;		    (sx (rsb "^Bpt ")
;;;			(fms "(lo) \\(0x[0-9a-f]*\\)" 1)))))

(defun get-field-offsets (name)
  (let* ((x (gdb-get (format "p %s" name)))
	 (x (sms "{\\([^}]*\\)" x 1))
	 (x (unconcat x ","))
	 (x (lmapcar i x (intern (sms "\\(\\sw*\\) *=" i 1))))
	 (fmt "p ((long) &%s.%s) - (long) &%s")
	 (y (lmapcar i x
	      (cons i (string-to-int
		       (sms "[0-9]*$" (gdb-get (format fmt name i name)))))))
	 )
    y))

