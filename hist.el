(load-library "chistory")

(define-key t-map "h" 'command-history-mode)

(setq default-command-history-filter-garbage
      (append default-command-history-filter-garbage
	      '(goto-line kill-buffer)))

(setq list-command-history 512)

(setq command-history-map (make-sparse-keymap))

(define-key command-history-map "ç"
  '(lambda () (interactive)
     (eval-defun (point))
     (kill-buffer (current-buffer))))

(setq command-history-hook
      '(lambda ()
	 (use-local-map command-history-map)
	 (toggle-read-only)))
