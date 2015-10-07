(defun n-chist ()
;  (setq buffer-read-only nil)
  )

(add-hook 'command-history-hook 'n-chist)

(define-key command-history-map "x" 'n-chist-pull)

; this does NOT work the way we would like it to

(defun n-chist-pull () (interactive)
  (copy-line-as-kill)
  (execute-kbd-macro (kbd "M-: C-y"))
  )

