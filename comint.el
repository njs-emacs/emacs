(load-standard "comint")

(defun process (&optional object)
  (cond ((processp object) object)
	((get-buffer-process (buffer object)))
	))

(defun send (proc s &optional insert) (interactive)
  (let ((proc (process proc)))
    (save-window-excursion
      (set-buffer (process-buffer proc))
      (goto-char (point-max))
      (set-marker comint-last-input-start (process-mark proc))
      (cond (insert (insert s "\n")
		    (comint-send-input))
	    ((process-send-string proc (concat s "\n")))
	    )
      (eob)
      (sit-for 1)
      ))
  )

(defvar comint-filter-stack nil)

(defun comint-filter-push (fun)
  (let ((proc (get-buffer-process (current-buffer))))
    (setq comint-filter-stack
	  (cons (process-filter proc) comint-filter-stack))
    (set-process-filter proc fun)))

(defun comint-filter-pop ()
  (let ((proc (get-buffer-process (current-buffer))))
    (set-process-filter proc (car comint-filter-stack))
    (setq comint-filter-stack (cdr comint-filter-stack)))
  )

(defun comint-filter-stack-enable ()
  )

(defun ns-comint-mode ()
  (make-local-variable 'comint-get-output)
  (make-local-variable 'comint-filter-stack)
  (setq comint-filter-stack nil)
  (setq next-line-add-newlines nil)
  )

(add-hook 'comint-mode-hook 'ns-comint-mode)

;;; comint-get
  
(defvar comint-get-output nil)

(defun comint-get-filter (proc string)
  (sx (set-buffer (process-buffer proc))
      (setq comint-get-output (concat comint-get-output string))
      )
  )

(defun comint-get (buffer s &optional match)
  (sx (set-buffer (setq buffer (buffer buffer)))
    (let (i (proc (get-buffer-process (current-buffer))))
      (setq match (or match comint-prompt-regexp))
      (comint-filter-push 'comint-get-filter)
      (setq comint-get-output "")
      (send buffer s nil)
      (unwind-protect
	  (while
	      (not (setq i (string-match match comint-get-output)))
	    (accept-process-output proc))
	(comint-filter-pop))
      (substring comint-get-output 0 i))
    ))


(defun comint-wait ()
  (while (sx (not (rsb comint-prompt-regexp)))
    (accept-process-output (process) 1))
  )

(defun comint-clean ()
  "Returns T if there are no non-blank characters following prompt."
  (sx (comint-bol nil) (looking-at "\\s *$"))
  )
  
(load-overrides "comint")

