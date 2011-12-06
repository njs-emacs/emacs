(setq float-output-format "%.2f")

(defun td (tp)
  (let* ((lo (min tp 2025))
	 (hi (- tp lo)))
    (+ (- (* lo .25) 13.34) (* hi .4))
    )
  )

(defun tax (x)
  (let* ((nie 170.54)
	 (nir (* x .102))
	 (tp (- x 287.42))
	 (td (td tp))
	 )
    (list nie nir (+ nie nir) tp td (- x nie td))
    ))

(tax 2114)
