; will expand later

(defun ruby-get-command ()
  (sx
   (cond
    ((sx (bol) (looking-at "##~"))
     (bs (match-end 0) (point$))
     )
    ((rsb "\\bdef\\s *\\([A-Za-z_][._0-9A-Za-z]*\\)")
     (format "--cmd=%s" (match-string 1))
     )
    )
   )
  )

(defun ruby-exec-this (&optional cmd) (interactive)
  (let ((command (ruby-get-command)))
    (compile (ruby-command (buffer-file-name) (list command)))
    )
  )

(defun ruby-mode-hook-ns ()
  (setq grep-spec "*.rb")
  (emacs-read-hash-plus)
  (define-key ruby-mode-map [M-f9] 'ruby-exec-this)
  (define-key ruby-mode-map [S-f9] 'ruby-exec-replay)
  (setq log-short-time-insert-format "%d_%H%M")
  (modify-syntax-entry ?_ "w")
  )

(add-hook 'ruby-mode-hook 'ruby-mode-hook-ns)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "loaded ... .extra/ruby-mode.el")
