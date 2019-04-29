(defvar emacs-exec-dir
  "d:/e/emacs"
  "The directory which holds the emacs environment")

(defvar emacs-lisp-dir
  (concat emacs-exec-dir "lisp")
  "The directory which holds the emacs lisp files")

(cond
 ((eq (wuft-get 'system-name) 'boo)
  (defun // (x)
    (cond
     ((replace-regexp-in-string "boo/\\(\\sw\\)" "\\1:" x))
     ((concat "//" x))
     )
    ))
  (t (defun // (x) (concat "//" x)))
  )

;(// "boo/e/.p/_/T/torrent/deluge/tc")

(defun bash-bin-set (dir)
  (setq bash-bin-root dir)
  (setq bash-file-name (format "%s/bash.exe" bash-bin-root))
  (setenv "ESHELL" bash-file-name)
  )

(bash-bin-set
  (filename-concat 
   (or (getenv "cgw")
;       (file-if-exists "c:/MinGW/msys/1.0")
;       (file-if-exists "d:/G/git")
       (locate-file-in-path "cygwin64" `("c:" "d:"))
       (locate-file-in-path "cygwin32" `("c:" "d:"))
       (locate-file-in-path "cygwin" `("c:" "d:"))
       )
   "bin"
   ))

