(defun nname (n)
  "Return a string representation of number N (N < 1000000).\nThe string is not always what you would call grammatically perfect."
  (let ((units '("" "one" "two" "three" "four" "five"
		 "six" "seven" "eight" "nine" "ten" "eleven" "twelve"
		 "thirteen" "fourteen" "fifteen" "sixteen" "seventeen"
		 "eighteen" "nineteen"))
	(tens '("" "" "twenty" "thirty" "forty" "fifty" "sixty"
		"seventy" "eighty" "ninety")))
    (cond
     ((= n 0) "zero")
     ((< n 20) (nth n units))
     ((< n 100) (format "%s %s" (nth (/ n 10) tens) (nth (mod n 10) units)))
     ((< n 1000) (concat (format "%s hundred" (nname (/ n 100)))
			 (if (not (zerop (mod n 100)))
			     (format " and %s" (nname (mod n 100))))))
     ((< n 1000000) (concat (format "%s thousand" (nname (/ n 1000)))
			    (if (not (zerop (mod n 1000)))
				(format " %s" (nname (mod n 1000))))))
     )))


