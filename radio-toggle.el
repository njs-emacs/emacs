;
; allow a single key to simultaneously uncomment a line and
; comment all other lines which are similarly tagged
;
;

(defun line-comment-set-state (state)
  (sx (bol) (kill-regexp "#*") (and state (insert "#")))
  )

(defun toggle-radio () (interactive)
 (let ((tag (sx (bol) (find-match-string "#~R{\\(.*\\)}"))))
  (sx (bob)
   (while (rsf tag)
     (line-comment-set-state t)
     )
   )
  (line-comment-set-state nil)
  ))


(setq toggle-radio-mode-keymap (make-sparse-keymap))

(define-key toggle-radio-mode-keymap (kbd "C-.") 'toggle-radio)

(define-minor-mode toggle-radio-mode
  "."
  :init-value nil
  :lighter " #TR#"
  :keymap toggle-radio-mode-keymap
  (cond 
   )
)
