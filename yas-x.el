;;; yas-x.el --- extension for mapping yasnippets to keys locally

;; Copyright (C) 2023 Otzo Software 

;; Author: Nick Steed <nick@otzo.org>
;; Version: 0.0.1
;; Package-Requires: ((yasnippet))
;; Keywords: emacs, yasnippet
;; URL: https://flamingant.github.io/yas-x

;;; Commentary:

;; yas-x is a yasnippet extension which provides locally mapping a 
;; key combination to a yasnippet

(require 'yasnippet)

(defvar yas-x-map nil "Expand map for yas-x")

(set-default 'yas-x-map (make-sparse-keymap))

(make-variable-buffer-local 'yas-x-map)

(defun yas-x-expand (name)
  "Do the low-level dirty work for yas-x-expand-command."
  (setq templates
    (mapcan #'(lambda (table)
		(yas--fetch table name))
	    (yas--get-snippet-tables)))
  (yas--expand-or-prompt-for-template templates)
  )

(defun yas-x-expand-command (arg) (interactive "p")
  "Expand the snippet that was mapped to the current command key."
  (let* ((keys (this-command-keys))
	 (binding (lookup-key yas-x-map keys))
	 )
    (cond
     (binding (yas-x-expand binding))
     )
    )
  )

;;;###autoload
(defun yas-x-define (key name)
  "Locally define KEY to invoke YASNIPPET."
  (local-set-key key 'yas-x-expand-command)
  (define-key yas-x-map key name)
  )

(provide 'yas-x)

;;; yas-x.el ends here
