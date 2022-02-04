; have clickable hot zones

(defun hot-remove () (interactive)
  (put-text-property 1 (1- (point-max)) 'invisible nil)
  )

(defun hot-eval (expr)
  (let* ((w (current-window))
	 (list (delete w
		       (get-buffer-window-list (current-buffer) nil 'visible)))
	 (window (car list))
	 (expr (read (format "(progn %s)" expr)))
	 )
    (cond
     (window
      (other-window 1)
      (bob)
      (eval expr)
      (other-window -1)
      )
     (t (eval expr))
     )
    )
  )
    

(defun hot-jump (&optional event)
  (interactive (list last-input-event))
  (let* ((pi (posn-point (event-end event)))
	 (expr (get-text-property pi 'expr)))
    (hot-eval expr)
    )
  )

(setq hot-jump-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'hot-jump)
    (define-key map [mouse-1] 'hot-jump)
    (define-key map [follow-link] 'mouse-face)
    (define-key map "\C-m" 'hot-jump)
    map)
)

(defun hot-fontify-after-match ()
 (let ((s0 (match-beginning 0)) (e0 (match-end 0))
       (s1 (match-beginning 1)) (e1 (match-end 1))
       (s2 (match-beginning 2)) (e2 (match-end 2))
       )
   (set-text-properties s0 e0 nil)
   (put-text-property s0 e0 'invisible t)
   (put-text-property s1 e1 'invisible nil)
   (put-text-property s1 e1 'expr (buffer-substring-no-properties s2 e2))
   (put-text-property s1 e1 'face 'link)
   (put-text-property s1 e1 'mouse-face 'highlight)
   (put-text-property s1 e1 'keymap hot-jump-map)
   )
 )

(defun hot-buffer-fontify () (interactive)
  (sx 
   (bob)
   (put-text-property 1 (1- (point-max)) 'invisible nil)
   (while (rsf "##\\(.*?\\)##\\(.*?\\)##")
     (hot-fontify-after-match)
     )
   )
  )

(defun hot-buffer-unfontify ()
 (remove-list-of-text-properties (point-min) (point-max)
				 '(invisible expr face mouse-face keymap))
 )

(define-minor-mode hot-mode
  "."
  :init-value nil
  :lighter " !H! "
  (cond 
   (hot-mode
    (hot-buffer-fontify)
    )
   (t
    (hot-buffer-unfontify)
    )
   )
  )

;; ##this## (message "OK") ##

;(hot-buffer-unfontify)
;(hot-buffer-fontify)

