(setq bash-bin-root "/bin")

(defun // (x) (concat "/" x))

(setq emacs-exec-dir "/usr/bin")
(setq emacs-lisp-dir "/usr/share/emacs/23.1/lisp")
(setenv "ESHELL" (format "%s/bash" bash-bin-root))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dos-filename-fix ()
  (setq filename
    (replace-regexp-in-string "\\([a-zA-Z]\\):" "//vom/\\1" filename))
  )

(defadvice find-file (before tweep) (dos-filename-fix))
(ad-activate 'find-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
