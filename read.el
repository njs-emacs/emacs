(defun readc () (read (current-buffer)))
(defun reads (s) (cond ((zerop (length s)) nil) ((read s))))

