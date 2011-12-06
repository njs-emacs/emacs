(defun c-fun-decl-to-ansi (pos) (interactive "d")
  (sx
   (let* ((start (c-fun-start pos))
	  (end (sxp (rsf "{")))
	  (fun (c-fun-decl:kr start)))
     (goto-char start)
     (kill-region start end)
     (insert (out-c-fun-decl:ansi fun) "\n{")
     )))

(defun c-fun-decl-to-kr (pos) (interactive "d")
  (sx
   (let* ((start (c-fun-start pos))
	  (end (sxp (rsf "{")))
	  (fun (c-fun-decl:ansi start)))
     (goto-char start)
     (kill-region start end)
     (insert (out-c-fun-decl:kr fun) "{")
     )))

(define-key global-map "\ec\ek" 'c-fun-decl-to-kr)
(define-key global-map "\ec\ea" 'c-fun-decl-to-ansi)

