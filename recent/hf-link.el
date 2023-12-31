; hf-link create clickable links which point to files
; largely experimental, plenty of scope for extension

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (dregf "properties" elfs "ever")
; (dregf "propert.*keymap" elfs "ever")
; no keymap property found with properties
;; hot?
; (dregf "hot-" elfs "ever")
; (dregf "hot-" "\\.org" "ever")

(defun thing-at-point--bounds-of-sexp ()
  (sx (cons (progn (thing-at-point--beginning-of-sexp) (point))
	    (progn (thing-at-point--end-of-sexp) (point))
	    )
      )
  )

(defun hf-bounds ()
;  (thing-at-point--bounds-of-sexp)
  (bounds-of-thing-at-point 'filename)
  )

(defun hf-mark (start end) (interactive "r")
  (cond
   ((use-region-p))
   ((let ((it (hf-bounds)))
      (setq start (car it))
      (setq end (cdr it))
      )
    )
   )
	 
  (let* ((sm (set-marker (make-marker) start))
	 (em (set-marker (make-marker) end))
	 )
    (put-text-property sm em 'hf-info `(hf-find-file ,sm ,em))
    (put-text-property sm em 'keymap hf-map)
    (put-text-property sm em 'face 'link)
    (put-text-property sm em 'mouse-face 'highlight)
    )
  )

(defun hf-marker-text (&optional pos)
  (interactive "d")
  (let ()
    (or pos (setq pos (point)))
    (debug)
    (setq p (text-properties-at pos))
    )
  )

(defun hf-find-file (start end)
  (let ((name (bs start end)))
    (find-file-other-window name))
  )

(defun hf-click (&optional event)
  (interactive (list last-input-event))
  (let* ((point (posn-point (event-end event)))
	 (plist (text-properties-at point))
	 (prop (plist-get plist 'hf-info))
	 )
    (eval prop)
    )
  )

(defun hf-get-link (&optional event)
  (interactive (list last-input-event))
  (let* ((point (posn-point (event-end event)))
	 (plist (text-properties-at point))
	 (prop (plist-get plist 'hf-info))
	 )
    prop
    )
  )

(defun hf-magit (&optional event)
  (interactive (list last-input-event))
  (let* ((link (hf-get-link event))
	 (name (bs (nth 1 link) (nth 2 link)))
	 )
    (magit name)
    )
  )

(defun hf-explore (&optional event)
  (interactive (list last-input-event))
  (let* ((link (hf-get-link event))
	 (name (bs (nth 1 link) (nth 2 link)))
	 )
    (explore name)
    )
  )

(defun hf-apply (fun &rest args)
  (let* ((event last-input-event)
	 (link (hf-get-link event))
	 (name (bs (nth 1 link) (nth 2 link)))
	 )
    (apply fun name args)
    )
  )

(defun hf-copy-to-kill () (interactive)
  (hf-apply '(lambda (x) (kill-new x)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def-key global-map (kbd "C-v C-m") 'hf-mark)

(setq hf-map (make-sparse-keymap))

(define-key hf-map (kbd "C-SPC") 'hf-click)
(define-key hf-map (kbd "C-m") 'hf-magit)
(define-key hf-map (kbd "C-x") 'hf-explore)
(define-key hf-map (kbd "C-k") 'hf-copy-to-kill)

(define-key hf-map [mouse-1] 'hf-click)
(define-key hf-map [mouse-2] 'hf-click)
(define-key hf-map [follow-link] 'mouse-face)

(defun hf-help (&optional event)
  (interactive (list last-input-event))
  (let* ((link (hf-get-link event))
	 (name (bs (nth 1 link) (nth 2 link)))
	 )
    (describe-symbol (intern name))
    )
  )

(define-key hf-map (kbd "h") 'hf-help)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(syntax-ppss)
;(get 'filename  'beginning-op)
