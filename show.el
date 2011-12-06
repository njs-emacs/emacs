(require 'reap)

(defun show-buf (buf contents &optional print)
  (set-buffer buf)
  (or print (setq print 'prin))
  (or (get-buffer-window-any-frame buf) (display-buffer buf))
  (erase-buffer)
  (dolist (i contents) (insert (funcall print i)))
  (setq major-mode 'show)
  (setq mode-name "show")
  (setq reap-protect nil)
  buf
  )

(setq show-buffer-name-function 'show-buffer-name-function)

(defun show-buffer-name-function ()
  "*show*"
  )

(defun show (&rest contents)
  (let ((buf (make-buffer (funcall show-buffer-name-function))))
    (show-buf buf contents)
    )
  )

(defun show1 (name &rest contents)
  (show-buf (or (and name (get-buffer-create name))
		(generate-new-buffer "*show*"))
	    contents)
  )

