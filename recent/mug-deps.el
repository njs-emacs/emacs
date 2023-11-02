; dependencies for mug.el

(defun alist-put (list tag val)
  "Like plist-put but acts on alists. Should really be built-in."
  (let ((cell (assoc tag list)))
    (cond
     (cell (setcdr cell val))
     ((setq list (cons (cons tag val) list)))
     )
    list)
  )

(defmacro sxp (&rest body)
  "Like save-excursion, but returns where point was at the end of the body execution."
  `(save-excursion ,@body (point)))

(defun point^ ()
  "The value of (point) at the start of the line."
 (sxp (beginning-of-line)))

(defun point$ ()
  "The value of (point) at the end of the line."
 (sxp (end-of-line)))

