(load-standard "isearch")

(defun isearch-yank-char ()
  "Pull next character from buffer into search string."
  (interactive)
  (isearch-yank-string (buffer-substring (point) (1+ (point))))
  )

(defun grep-extern (s)
  (grep (format "grep %s \"extern.*%s.*;\" *.h" grep-flags s))
  )

(defun isearch-extern () (interactive)
  (isearch-done)
  (grep (format "grep %s \"extern.*%s.*;\" *.h" grep-flags isearch-string))
  )

(defun isearch-ngrep () (interactive)
  (isearch-done)
  (ngrep isearch-string)
  )

(defun isearch-ngrep-i () (interactive)
  (isearch-done)
  (ngrep isearch-string nil "-in")
  )

(defun isearch-mark-find () (interactive)
  (or (rsf "/\\*\\*/") (progn (bob) (rsf "/\\*\\*/")))
  )

(defun ns-isearch-mode-hook ()
  (define-key isearch-mode-map "\C-c" 'isearch-yank-char)
  (define-key isearch-mode-map "\C-t" 'isearch-toggle-case-fold)
  (define-key isearch-mode-map "\M-g" 'isearch-ngrep)
  (define-key isearch-mode-map "\M-x" 'isearch-extern)
  (define-key isearch-mode-map "\M-i" 'isearch-ngrep-i)
  (define-key isearch-mode-map "\C-q" 'isearch-mark-find)

  (define-key isearch-mode-map [(f1)] nil)
  )

(add-hook 'isearch-mode-hook 'ns-isearch-mode-hook)

(setq search-ring-max 64)
(setq regexp-search-ring-max 64)
