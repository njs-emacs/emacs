(defvar chosig-alist
  '((Info-mode . "#ddd")
    ("scratch" . "#fee")
    (emacs-lisp-mode . "#eee"))
  "Alist matching major modes or buffer names with background colors.
Every cons cell on the alist has the form (CHECK . COLOR) where CHECK
is either a symbol matching the `major-mode' or a regular expression
matching the `buffer-name' of the current buffer. COLOR is a string
representing a valid color, eg. \"red\" or \"f00\".")

(defun chosig-choose-background ()
  "Pick background-color according to `chosig-alist'.
The overlay used is stored in `chosig-background'."
  (let ((alist chosig-alist)
	background)
    (while (and (not background) alist)
      (if (or (and (stringp (caar alist))
		   (string-match (caar alist) (buffer-name)))
	      (eq major-mode (caar alist)))
	  (setq background (cdar alist))
	(setq alist (cdr alist))))
    ;; cleanup
    (mapc (lambda (o)
	    (when (overlay-get o 'chosig)
	      (delete-overlay o)))
	  (overlays-in (point-min) (point-max)))
    ;; new one
    (when background
      (let ((o (make-overlay (point-min) (point-max)
			     (current-buffer) nil t)))
	(overlay-put o 'face `(:background ,background))
	(overlay-put o 'chosig t)))))

(add-hook 'after-change-major-mode-hook 'chosig-choose-background)
