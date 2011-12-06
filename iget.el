(defun iget-check (w prop)
  (or (iget w prop) (error "cannot get property %s for symbol %s"
			   (symbol-name prop) (symbol-name w))))

(defun iget (w prop &optional original)
  (and w (or (get w prop) (iget (get w 'super) prop w))))

