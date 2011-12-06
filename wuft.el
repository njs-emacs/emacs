(setq load-overrides-path-list nil)

(defun cons-load-path (s)
  (setq load-path (adjoin (expand-file-name s) load-path)))

(defun override-adjoin (dir) 
  (setq load-overrides-path-list
    (adjoin-accessible-directory dir load-overrides-path-list))
  )

(defun load-standard (file)
  (let ((load-path standard-load-path))
    (load file nil t)
    ))

(defun wuft-get (tag)
  (alist-get wuft-alist tag)
  )

(defun wuft-put (tag value)
  (setq wuft-alist (alist-put wuft-alist tag value))
  )

(defun safe-for-path (s)
  (replace-regexp-in-string "/" "-" s)
  )

(defun wuft-load-keys (file &rest keys)
  (mapcar '(lambda (x)
	       (load (format ".wuft/%s--%s-%s" file x (wuft-get x)) t t))
	    keys)
  )

(defun wuft-load (file)
  (apply 'wuft-load-keys file (mapcar 'car wuft-alist))
  )

(defun load-overrides (file)
  (wuft-load file)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq wuft-alist
  `(
    (location . nil)
    (emacs-version . ,emacs-major-version)
    (system-type . ,(intern (safe-for-path (symbol-name system-type))))
    (system-name . ,(intern (downcase (system-name))))
    )
  )

(cond
 ((string-match "dreamhost" (symbol-name (wuft-get 'system-name)))
  (wuft-put 'location 'dreamhost)
  )
 ((wuft-put 'location 'egham)
  )
 )
  
