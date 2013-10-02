(setq default-frame-alist nil)

(fset 'get-buffer-window
      (list 'lambda '(buf &optional frame)
	    (list 'funcall
		  (symbol-function 'get-buffer-window) 'buf (or 'frame t))))

(defun get-buffer-window-any-frame (buf)
  (car (delete nil (mapcar '(lambda (x) (get-buffer-window buf x))
			   (filtered-frame-list 'identity)))))

(defun default-frame-alist-put (tag val)
  (setq default-frame-alist (alist-put default-frame-alist tag val))
  )

(defun font-name-default-size (size)
  (format font-name-default-template size)
  )

(defun font-size (size)
  (set-frame-font (font-name-default-size size))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-overrides "frame-default")

(setq default-frame-alist
  (alist-merge default-frame-alist
	       `(
;		 (font . ,(font-name-default-size 11))
		 )
	       ))

(setq initial-frame-alist (copy-alist default-frame-alist))

(load-overrides "frame-initial")

(menu-bar-mode 0)

;(frame-parameters)
;(print (frame-parameters))

;(font-size 16)

;(set-frame-parameter (selected-frame) 'height 100)
;(set-frame-parameter (selected-frame) 'background-color "LightGray")
;(set-frame-parameter (selected-frame) 'background-color "Black")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
