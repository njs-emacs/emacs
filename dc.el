(defun dc (s)
  (substring (shell-output (concat "dc <<.\n" s "\n.")) 0 -1))

(defmacro dc- (a b)
  (dc (concat "16i10o" (upcase (symbol-name a)) " " (upcase (symbol-name b))
	      "-p")))
