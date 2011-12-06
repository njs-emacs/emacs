(defun rectangle-put-text-property (start end property value)
  ""
  (interactive "")
  (operate-on-rectangle
   '(lambda (a b c)
      (put-text-property a (point) property value)
      ) start end nil
   )
  )

(load "rect")

(define-key global-map "\C-v\C-a"
  '(lambda (start end) (interactive "r")
     (rectangle-put-text-property start end 'face 'secondary-selection)
     )
  )
