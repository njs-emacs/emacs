(defun define-key-marker (k m)
  (let ((x (assoc k key-markers)))
    (if x (setcdr x (list m))
      (setq key-markers (cons (list k m) key-markers))))
  (define-key global-map k 'goto-key-marker)
  )

(defun define-key-marker-dot (k) (interactive "kKey: ")
  (define-key-marker k (dot-marker)))

(defun goto-key-marker () (interactive)
  (let* ((k (this-command-keys))
	 (x (assoc k key-markers))
	 (m (car (cdr x))))
    (if (symbolp m) (setq m (eval m)))
    (if (markerp m)
	(let ((buf (marker-buffer m)))
	  (if buf
	      (progn
		(cond ((eq buf (current-buffer)))
		      ((pop-to-buffer buf)))
		(goto-char m))
	    (progn
	      (delq x key-markers)
	      (global-unset-key k)
	      (message "no buffer - marker key removed")))
      (goto-char m)))))

(setq key-markers nil)

