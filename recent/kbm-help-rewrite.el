(defun kbm-help-rewrite ()
  (interactive)
  (sx
   (set-buffer "*Help*")
   (read-only-mode 0)
   (bob)
   (while (rsf "C-[{}!:$~<>?/*_+^]")
     (let ((s (ms 0)))
       (delete-region (mb 0) (me 0))
       (insert (format "[%c]" 
		       (car (rassoc s function-key-to-kp-alist))))
       )
     )
   (read-only-mode 1)
   )
  )

(def-key global-map (kps "3-") 'kbm-help-rewrite)
     
