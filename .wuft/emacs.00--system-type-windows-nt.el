(setq bash-bin-root
  (filename-concat 
   (or (getenv "cgw")
       (locate-file-in-path "cygwin" `("c:" "d:")))
   "bin"
   ))

(setq emacs-exec-dir "d:/e/emacs-23/emacs")
(setq emacs-lisp-dir (concat emacs-exec-dir "/lisp"))
(setq bash-file-name (format "%s/bash.exe" bash-bin-root))
(setenv "ESHELL" bash-file-name)
