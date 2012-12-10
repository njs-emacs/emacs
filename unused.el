
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun completion--some (fun xs)
  "Apply FUN to each element of XS in turn.
Return the first non-nil returned value.
Like CL's `some'."
  (let ((firsterror nil)
        res)
    (while (and (not res) xs)
      (condition-case err
          (setq res (funcall fun (pop xs)))
        (error (unless firsterror (setq firsterror err)) nil)))
    (or res
        (if firsterror (signal (car firsterror) (cdr firsterror))))))

