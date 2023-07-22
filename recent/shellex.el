(defun shell-execute-text* (s plist)

  (let* ((nvc-global-enable nil)
	 (shell-program (or (plist-get plist :shell-program) "bash"))
	 (output-buffer-name (or (plist-get plist :output-buffer-name) "*shell-execute*"))
	 (output-buffer (get-buffer-create output-buffer-name))
	 shell-command
	 shell-output
	 (exec-file-name (or (plist-get plist :file-name)
			     (format "%s/%s-%d.tmp" (temporary-file-directory) shell-program (emacs-pid))))
	 exec-buffer
	 temp-output-buffer
	 )

    (cond
     ((string= shell-program "powershell")
      (let ((shell-args (or (plist-get plist :shell-args) "")))
	(setq exec-file-name (slash-back (format "%s.ps1" exec-file-name)))
	(setq shell-command (format "%s -Noninteractive -Command %s" shell-program exec-file-name))
	)
      )
     ((string= shell-program "cmd")
      (setq exec-file-name (slash-back (format "%s.bat" exec-file-name)))
      (setq shell-command (format "%s /c %s" shell-program exec-file-name))
      )
     (t
      (setq shell-command (format "%s %s" shell-program exec-file-name))
      )
     )

    (save-excursion
      (setq exec-buffer (find-file-noselect exec-file-name))
      (set-buffer exec-buffer)
      (set-buffer-file-coding-system 'unix)
      (erase-buffer)
      (insert s)
      (write-region nil nil exec-file-name nil 0)
      (set-buffer-modified-p nil)
      )

    (setq temp-output-buffer (get-buffer-create (format " *shell-execute-%s" (format-time-string "%s%3N"))))
    (shell-command shell-command temp-output-buffer)
    (setq shell-output (get-buffer-string temp-output-buffer))
    (kill-buffer temp-output-buffer)

    (set-buffer output-buffer)
    (or (plist-get plist :at-bob)
	(plist-get plist :at-eob)
	(plist-get plist :no-erase)
	(erase-buffer)
	)

    (cond
     ((plist-get plist :at-bob) (bob))
     ((plist-get plist :at-eob) (eob))
     (t (bol))
     )

    (insert shell-output)

    (cond
     ((plist-get plist :show)
      (display-buffer-use-some-window output-buffer nil)
      )
     ((plist-get plist :display)
      (display-buffer output-buffer (plist-get plist :display) (plist-get plist :display-frame))
      )
     ((plist-get plist :kill)
      (kill-buffer output-buffer)
      )
     ((replace-buffer-in-windows output-buffer))
     )
    shell-output
    )
  )

(defun shell-execute-text (s &rest plist)
  (save-excursion (shell-execute-text* s plist))
  )

(defun shell-execute-region-text (start end)
  (interactive "r")
  (shell-execute-text (bs start end))
  )

