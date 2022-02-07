(defun vc-git--quick-commit (m) (interactive "sMessage: ")
  (vc-git--call nil "commit" "-m" m)
  )

(def-key c-rbracket-map (kbd "C-c") 'vc-git--quick-commit)
