(cond
 ((eq emacs-major-version 21)
  (setq emacs-exec-dir "/usr/bin")
  (setq emacs-lisp-dir "/usr/share/emacs/21.4/lisp")
  )
 ((eq emacs-major-version 23)
  (setq emacs-exec-dir (format "%s/%s/" (getenv "HOME") "/local/bin"))
  (setq emacs-lisp-dir (concat emacs-exec-dir "/lisp"))
  (menu-bar-mode 0)
  )
 )

(setq perl-e-path "~/perl")
(setq backup-root "~/_backup")
(setq apache-home "~")

