(defun doc-start ()
  (setq section nil)
  )

(defun underline (s)
  (concat s "\n" (make-string (length s) ?=)))

(defun section (header)
  (if (null section) (setq section (list 0)))
  (setq section (cons (1+ (car section)) (cdr section)))
  (concat
   (underline
    (concat
     (docat i (reverse section) "" (format "%d." i))
     "  " header))
     "\n"
   ))

(defun section-in (&optional header)
  (setq section (cons 0 section))
  (and header (section header))
  )

(defun section-out (&optional header)
  (setq section (cdr section))
  (and header (section header))
  )


