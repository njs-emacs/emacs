(defvar emacs-exec-dir
  "d:/e/emacs"
  "The directory which holds the emacs environment")

(defvar emacs-lisp-dir
  (concat emacs-exec-dir "lisp")
  "The directory which holds the emacs lisp files")

(defun // (x) (concat "//" x))

(setq bash-bin-root
  (filename-concat 
   (or (getenv "cgw")
       (locate-file-in-path "cygwin" `("c:" "d:")))
   "bin"
   ))

(setq bash-file-name (format "%s/bash.exe" bash-bin-root))
(setenv "ESHELL" bash-file-name)
