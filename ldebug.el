(defun current-defun-name () (interactive)
  (save-excursion
    (beginning-of-defun)
    (down-list 1)
    (forward-sexp 1)
    (buffer-substring (1+ (point)) (progn (forward-sexp 1) (point)))))

(defun debug-current-defun () (interactive)
  (debug-on-entry (intern (current-defun-name))))

(defun cancel-debug-current-defun () (interactive)
  (cancel-debug-on-entry (intern (current-defun-name))))

(define-key l-map "d" 'debug-current-defun)
(define-key l-map "D" 'cancel-debug-current-defun)
