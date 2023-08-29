(require 'reap)

(defun show-buf (buf contents &optional print append)
  "Display output in a 'show' buffer."
  (or print (setq print 'prin))
  (or (get-buffer-window-any-frame buf) (display-buffer buf))
  (sx 
   (set-buffer buf)
   (cond
    (append
     (end-of-buffer)
     (and (stringp append) (insert append))
     )
    (t (widen) (kill-region (point-min) (point-max)))
    )
   (dolist (i contents) (insert (funcall print i)))
   (setq major-mode 'show)
   (setq mode-name "show")
   (setq reap-protect nil)
   )
  buf
  )

(defun show-buffer-name-function ()
  "*show*"
  )

(setq show-buffer-name-function 'show-buffer-name-function)

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

