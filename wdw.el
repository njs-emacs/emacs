;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dbg-mode () (interactive)
  (setq mode-name "dbg")
  (setq major-mode 'dbg-mode)
  (emacs-read-hash-plus)
  )

(defun start-perl () (interactive)
  (save-buffer)
  (start-process "*sex*" nil "perl" (format "%s" (buffer-file-name)))
  )

(defun start-wdw () (interactive)
  (save-buffer)
  (start-process "*sex*" nil "wdw" (format "/in=%s" (basename)))
  )

(defun start-nl () (interactive)
  (save-buffer)
  (start-process "*sex*" nil nl-exec-program (buffer-file-name))
  )

