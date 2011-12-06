;#~ experimental

(setq foq (make-face 'foq))
(set-face-foreground foq "yellow")
(set-face-background foq "red")

(defun hl () (interactive)
  (put-text-property (point^) (point$) 'face 'foq))
(global-set-key [f7] 'hl)

(defun hl (faces)
  (let* ((face (get-text-property (point^) 'face))
	 (x (or (cadr (memq face faces)) (car faces)))
	 )
    (put-text-property (point^) (point$) 'face x)
    ))

(global-set-key [f7]
  '(lambda () (interactive) (hl '(default highlight region secondary-selection)))
  )

(global-set-key [f13]
  '(lambda () (interactive) (hl '(highlight secondary-selection default)))
  )


