(defun explore (dir) (interactive "DExplore directory: ")
  (shell-command
   (format "explorer /e,%s"
	   (string-sub (expand-file-name dir) "/" "\\"))
   ))

(global-unset-key [f2])
(global-set-key [f2] 'explore)

;; for nvc.el

(setq nvc-remote-ntfs nil)

;; for php.el

;(setq php-exe "d:/p/php5/php.exe")
(setq php-exe "d:/p/php5.3/php.exe")

; old version using own functions
(defun cygdrive-to-dos (string)
  (string-sub string "/cygdrive/\\(.\\)/"
      '(format "%s:/" (string-match-region string 1)))
  )

; new version using canonical emacs functions
(defun cygdrive-to-dos (string)
  (replace-regexp-in-string "/cygdrive/\\(.\\)/"
      "\\1:/" string)
  )
