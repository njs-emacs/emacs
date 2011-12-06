;#~:c code editing
;
; functions to mark and move c functions
; may work for perl too.
;

(defun ps-fun ()
  "location of the start of a function definition"
  (sxp (up-list (- (depth)))
    (cond ((rsb "}") (fl 1))
	  ((rsb "^\\S ") (rsb "^$") (rsb ";") (fl 1))
	  )
    ))

(defun pe-fun ()
  "location of the end of a function definition"
  (sxp (up-list (- (depth))) (MB (rsf "{")) (fx 1) (fl 1)))

(defun rg-fun ()
  "the range of a function definition"
 (list (ps-fun) (pe-fun)))

(defun move-function (to)
  (kill-region (ps-fun) (pe-fun))
  (gc to)
  (yank)
  )

(defun function-first-use (name)
  (sx (bob) (rsf (format "%s\\s *(" name))))

(defun move-function-above-first-use (name) (interactive)
  (let* ((to (sxp (gc (function-first-use name)) (gc (ps-fun)))))
    (move-function to))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun goto-last-include () (eob) (rsb "^#include") (fl 1))

