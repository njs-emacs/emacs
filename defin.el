(mapcar 'defin '(ilambda idefun defun-key))

(defin 'interactive)
(defin 'report)
(defin 'xt-widget)

(defin 'setq 'defun)

(defin 'mapautoload 'defun)

(defin 'map-markers 'defun)

(defin 'dofiles 'defun)
(defin 'dofile-pairs 'defun)

(defin 'with-file 'defun)
(defin 'make-file 'defun)
(defin 'save-cd 'defun)

(defin 'mvb 'defun)
(put 'case 'lisp-indent-hook 'defun)
(put 'dolist 'lisp-indent-hook 'defun)
(put 'dotimes 'lisp-indent-hook 'defun)

(defin 'with-special-binding 'defun)

(mapcar '(lambda (x) (defin x 'defun)) '(sxp swxo sr))

(defin 'q 'defun)

(defin 'if-out 'defun)
(defin 'buffer-replace 'defun)
(defin 'buffer-replace-lines 'defun)
(defin 'map-search 'defun)

(defin 'cfun 'defun)
(defin 'case-match 'defun)

(defin 'wgrep 'defun)
(defin 'grep-map 'defun)

(defin 'map-cmarkers-bogus 'defun)
(defin 'map-cmarkers-quick 'defun)
(defin 'map-cmarkers 'defun)

(defin 'dolist)
(defin 'dotimes)
