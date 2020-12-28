(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(fringe ((t (:background "#E0E0C0" :foreground "#A0A0A0"))))
 '(highlight-changes-face ((((class color)) (:background "yellow" :foreground "red")))))

(setq default-frame-alist
  (alist-merge default-frame-alist
	       `(
		 (background-color . "LightGray")
		 (left . 800)
		 (top . 1)
		 )
	       )
  )

;(cond
; ((eq (wuft-get 'system-name) 'red)
;  (setq default-frame-alist (alist-merge default-frame-alist `((height . 88))))
;  )
; (t
;  (setq default-frame-alist (alist-merge default-frame-alist `((height . 100))))
;  )
; )

(defun display-pixel-height-fixed (&optional x)
;  1200
;  (display-pixel-height)
  (nth 3 (alist-get  'geometry (nth 0 (display-monitor-attributes-list))))
  )

(let ((height (/ (display-pixel-height-fixed) 12))
      )
  (setq default-frame-alist (alist-merge default-frame-alist `((height . ,height))))
  )

