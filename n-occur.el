(defun occur-parse-buffer () (interactive)
  (let (pat buffer)
    (bob)
    (debug)
    (rsf "matches total for \"\\(.*\\)\":")
    (setq pat (match-string-no-properties 1))
    (while (rsf "matches in buffer: \\(.*\\)")
      (setq buffer (match-string-no-properties 1))
      (while (rsf pat)
	;; this is making progress but there is too much functionality
	;; to add to finish, and there are simpler ways of achieving
	;; the same aim
	)
      )
    )
  )

(define-key global-map (kbd "s-z") 'occur-parse-buffer)

(multi-occur-in-matching-buffers "\\.c" "ua_\\sw+" nil)

(buffer-filter-by-name "\\.c")

(occur-1 "ua_\\sw+" 3 (buffer-list))

(occur-1 "ua_\\sw+" 0 (buffer-filter-by-name "\\.c"))
