(define-key global-map [<kp-add>] 'describe-function)
(define-key global-map [M-s-mouse-1] 'm-describe-function)

(defun m-describe-function (click)
  (interactive "e")
  (save-excursion
    (mouse-set-point click)
    (let ((fn (function-called-at-point)))
      (describe-function fn)
      )
    )
  )
