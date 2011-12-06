(defun window-select-top ()
  (while (mvb (x y) (window-edges) (> (+ x y) 0))
    (other-window 1)))

(defun window-line ()
  (count-lines (window-start) (1+ (point))))

(defun window-enforce-bottom (n)
  (let ((nn (- (window-height) (window-line))))
    (cond ((> n nn) (recenter)))))

(defun goto-marker (marker)
  (set-buffer (marker-buffer marker))
  (goto-char marker)
  (marker-buffer marker)
  )
