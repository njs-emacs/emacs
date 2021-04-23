(defvar def-key-map-alist nil "Reverse mapping of keymap to prefix")
(defvar def-key-pretty nil "Format elements in def-key-show in even columns")
(defvar def-key-sort nil "Sort order def-key-show")

(defun def-key-map (key map)
  (setq def-key-map-alist (alist-put def-key-map-alist map key))
  )

(defvar def-key-history nil "Saved history of unevalled args to def-key")

(defmacro def-key (keymap key def)
  "Identical in functionality to define-key, with the side-effect that the
 definition is logged (unevaluated) to a list which gets saved on exit
 or on demand"
  (eval `(setq def-key-history
	   (cons (list ',keymap ',key ',def) def-key-history)))
  `(progn 
     (define-key ,keymap ,key ,def)
     )
  )

(defmacro def-key-global (key def)
  `(def-key global-map ,key ,def)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun def-key-description (x)
  (cond 
   ((or (stringp x) (vectorp x))
    (format "(kbd \"%s\")" (key-description x))
    )
   (t (format "%S" x))
   )
  )

(defun def-key-item (x)
  (let* ((map (nth 0 x))
	 (key (key-description (eval (nth 1 x))))
	 (rb (or (alist-get map def-key-map-alist) map))
	 )
    (format "%s %s" rb key))
  )

;(def-key-item '(c-lbrace-map (kbd "#") 'ace-jump-buffer))
;(def-key-item '(c-comma-map (kbd "C-SPC") 'mc/edit-lines))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun def-key-fill-buffer1 (list &optional sort)
  (let* ((buffer (get-buffer-create "*def-key*"))
	 )
    (cond
     (sort (setq list (sort (copy-sequence list) sort)))
     )
    (set-buffer buffer)
    (erase-buffer)
    (mapcar
     '(lambda (x)
	(condition-case erc
	    (let ()
	      (insert
	       (format
		(cond
		 (def-key-pretty "(def-key %-20s %-20s '%S)\n")
		 (t "(def-key %s %s '%S)\n")
		 )
		 (nth 0 x)
		(def-key-description (nth 1 x))
		(eval (nth 2 x)))))
	  (error nil)
	  ))
	list)
    (bob)
    buffer
    )
  )

(defun def-key-fill-buffer () (interactive)
  (def-key-fill-buffer1 def-key-history def-key-sort)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun def-key-sort-binding-compare (a b)
  (string< (prin1-to-string (nth 2 a))
	   (prin1-to-string (nth 2 b))))

(defun def-key-sort-binding ()
  (interactive)
  (setq def-key-sort 'def-key-sort-binding-compare)
  (def-key-fill-buffer)
  (message "sort by binding")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun def-key-sort-key-compare (a b)
  (string< (def-key-item a)
	   (def-key-item b)))

(defun def-key-sort-key ()
  (interactive)
  (setq def-key-sort 'def-key-sort-key-compare)
  (def-key-fill-buffer)
  (message "sort by key")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun def-key-pretty ()
  (interactive)
  (setq def-key-pretty (not def-key-pretty))
  (def-key-fill-buffer)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun def-key--show ()
  (interactive)
  (switch-to-buffer (def-key-fill-buffer))
  (def-key-mode)
  )

(defun def-key-occur-kps ()
  (interactive)
  (occur "kps")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun def-key--save ()
  (interactive)
  (let* ((name (daily-month-path
	       (format-time-string "key-def--save-%y%m%d-%H%M%S.eel"))
	      )
	 (def-key-pretty nil)
	 (buffer (def-key-fill-buffer))
	)
    (save-excursion (set-buffer buffer) (write-file name))
    (message "written key-def history to %s" name)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq def-key-mode-keymap (make-sparse-keymap))

(define-key def-key-mode-keymap (kbd "C-c C-v") 'def-key-sort-key)
(define-key def-key-mode-keymap (kbd "C-c C-b") 'def-key-sort-binding)
(define-key def-key-mode-keymap (kbd "C-c C-x") 'def-key-pretty)
(define-key def-key-mode-keymap (kbd "C-c C-c") 'def-key-fill-buffer)
(define-key def-key-mode-keymap (kbd "C-c C-k") 'def-key-occur-kps)
(define-key def-key-mode-keymap (kbd "C-c C-s") 'def-key--save)

(define-minor-mode def-key-mode
  "."
  :init-value nil
  :lighter " !C"
  :keymap def-key-mode-keymap
  (cond
   (def-key-mode
    (message "on")
    )
   ((message "off")
    )
   )
)

;
;  (lookup-key global-map (kbd "C-!"))
;  (where-is c-bang-map)
;  (where-is c-comma-map)
;  (where-is c-rbrace-map)
;  (where-is c-gt-map)
; (key-description (car (where-is-internal c-bang-map)))
;(describe-variable 'def-key-history)

(add-hook 'kill-emacs-hook 'def-key--save)

;; (dregf "define-key" elfs "ever")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO
;; preserve the unevalled key spec rather than the actual keys
;; 
