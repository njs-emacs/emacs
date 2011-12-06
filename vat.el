(setq float-output-format "%.2f")
(defun vat-ex (x) (/ x 1.175))
(defun vat-add (x) (+ (/ x .175)))

(vat-ex 59.50)
(vat-ex 119.00)
