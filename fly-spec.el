;;; parsing is not very robust, but can be strengthened later


(defun fly-spec-read-string ()
  (sx
   (bob)
   (let (start end)
     (cond ((and
	     (setq start (rsf "{#~{"))
	     (setq end (rsf "}~#}"))
	     )
	    (setq s (bs start (- end 4)))
	    (string-substitute s 10 32)
	    )
	   )
     )
   )
  )

(defun fly-spec-parse (s)
  (let (k v x o)
    (while (string-match "\\s *\\(\\S +\\)\\s *=>\\s " s)
      (setq k (intern (substring s (match-beginning 1) (match-end 1))))
      (setq s (substring s (match-end 0)))
      (setq x (read-from-string s))
      (setq v (car x))
      (setq s (substring s (cdr x)))
      (setq o (cons (cons k v) o))
      )
    o
    )
  )

(defun fly-spec-read ()
  (let (s)
    (cond
     ((setq s (fly-spec-read-string))
      (fly-spec-parse s)
      )
     )
    )
  )

(defun fly-spec-get (prop)
  (let ((p (fly-spec-read)))
    (gget p prop)
    )
  )
