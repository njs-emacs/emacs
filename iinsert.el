(defun iinsert ()
  "Interactively insert text, using recursive-edit.
On exit, return t if anything was inserted at the original point.
Point will always return to original point on exit."
  (interactive)
  (let ((< (dot-marker))
	(> (dot-marker)))
    (move-marker > (1+ (marker-position >)))
    (recursive-edit)
    (goto-char (1- (marker-position >)))
    (not (= (1+ (marker-position <)) (marker-position >)))
    ))

