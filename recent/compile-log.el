(defun compile-write-log* (text file)
  ;; obsolete version
  (cond ((file-exists-p file))
	((write-string-to-file
	  file "-*- mode: compilog;\n##version:1##\n##nologvisit##\n##nobackup##\n"))
	)
  (write-file-and-refresh-if-visiting
   file
   (concat "################################################################\n" text)
   )
  )

(defvar compile-text-length-threshold 100000 "Limit of unconfirmed compile log write")
;(setq compile-text-length-threshold 200000)

(defun compile-write-log* (text dir file)
  (cond ((file-exists-p dir))
	((make-directory dir))
	)
  (let ((edit (and (> (length text) compile-text-length-threshold)
		   (y-or-n-p "Do you want to edit this compilation log first? "))))
    (write-string-to-file-edit-maybe file text edit)
    )
  )

(defun compile-write-log (buffer msg)
  (let* ((text (buffer-string))
	 (dir (format "e:/#capture/emacs/compile/%s" (format-time-string "%y%m%d")))
	 (file (format "%s/%s.clog" dir (format-time-string "%y%m%d-%H%M%S")))
	)
    (cond ((string-match (concat "##" "nolog##") text))
	  ((compile-write-log* text dir file))
	  )
    )
  )

(defvar compilation-original-buffer nil "Buffer that invoked this compilation")

(defun compilation-original-plist ()
  (cond
   ((bufferp compilation-original-buffer)
    (buffer-local-value 'compile-log-plist compilation-original-buffer)
    )
   )
  )

(defun compile-log-enable ()
  (let* ((plist (compilation-original-plist)))
    (setq compilation-finish-functions (delete 'compile-write-log compilation-finish-functions))
    (cond
     ((alist-get 'disable plist))
     ((setq compilation-finish-functions (adjoin 'compile-write-log compilation-finish-functions)))
     )
    )
  )

(setq compilation-mode-hook (adjoin 'compile-log-enable compilation-mode-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro setq-verbose (sym value)
  `(prog1 (setq ,sym ,value)
     (message "%s set to %s" ',sym ,value)
     )
  )

(defun compilation-dwim ()
  (interactive)
  (sx
   (cond
    ((sx (bol)
	 (rsf "^-\\*- mode: compilation; default-directory: "))
     (gme 0)
     (let ((s (readc)))
       (setq-verbose default-directory s)
       )
     )
    )
   )
  )
  
(define-key compilation-mode-map (kbd "C-c C-x") 'compilation-dwim)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(makunbound  'compile-log-plist)
(defvar compile-log-plist nil "Control compilation logging")
(make-variable-buffer-local 'compile-log-plist)
(set-default 'compile-log-plist nil)

;(setq compile-log-plist nil)
;(setq compile-log-plist `((disable . t)))

;(describe-variable 'compile-log-plist)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq compilog-mode-keymap (make-sparse-keymap))

(define-minor-mode compilog-mode
  "."
  :init-value nil
  :lighter " col"
  :keymap compilog-mode-keymap
  (cond 
   )
)

(define-key compilog-mode-keymap (kbd "M-RET") 'compilog-replay)

(defun compilog-replay () (interactive)
  (let* (
	 (start (sxp (rsb "^-\\*- mode.*\"\\(.*\\)\"")))
	 (wd (ms 1))
;	 (start (sxp (rsb "Compilation started")))
	 (end (sxp (rsf "Compilation \\(exited\\|finished\\)") (fl 1) (bol)))
	 (s (buffer-substring-no-properties start end))
	 (name
	  (format "%s-%s"
		  (substring (file-name-nondirectory (buffer-file-name)) 0 6)
		  start))
	 (buffer (make-buffer (format "*compilog-%s*" name) s))
	 )
    (switch-to-buffer buffer)
    (compilation-mode)
    (setq default-directory wd)
    (setq compilation-last-buffer buffer)
    )
  )
