(setq ahk-exec-program "d:/a/autohotkey/autohotkey.exe")

(defun ahk-mode ()
  (interactive)
  (setq major-mode 'ahk-mode)
  (setq mode-name "AHK")
  )

(put 'ahk-mode 'eval-buffer-modal 'ahk-buffer-eval)

(defun ahk-buffer-eval-shell ()
  (shell-command
   (format "%s \"%s\"" ahk-exec-program (buffer-file-name))
   )
  )

(defun ahk-buffer-eval-compile ()
  (compile
   (format "%s \"%s\"" ahk-exec-program (buffer-file-name))
   )
  )

(defun ahk-buffer-eval ()
  (save-buffer)
  (ahk-buffer-eval-shell)
  )

(add-mode "\\.ahk$" 'ahk-mode)

(defun ahk-execute-string (s))
