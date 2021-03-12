(defun c-quote-map-init ()
  (setq c-quote-map (copy-keymap ctl-x-r-map))

  (global-set-key (kbd "C-'") c-quote-map)

  (def-key c-quote-map (kbd "E") 'keydef-edit-init)

  (def-key c-quote-map (kbd "C-k") 'kmacro-to-register)
  (def-key c-quote-map (kbd "C-'") 'bookmark-jump)
  (def-key c-quote-map (kbd "C-;") 'bookmark-set)
  (def-key c-quote-map (kbd "C-/") 'bookmark-bmenu-list)

  (def-key c-quote-map (kbd "C-#") 'ace-jump-line-mode)
  (def-key c-quote-map (kbd "C-.") 'ace-jump-buffer)
  (def-key c-quote-map (kbd "C-=") 'ace-window)
  (def-key c-quote-map (kbd "C-w") 'ace-jump-word-mode)

  (def-key c-quote-map (kbd "C-j") 'jump-to-register)
  
  (def-key c-quote-map (kbd "C-b") 'recentf-open-files)
  (def-key c-quote-map (kbd "C-c") 'command-history)

  (def-key c-quote-map (kbd "C-n") 'mc/mark-next-like-this)
  (def-key c-quote-map (kbd "C-p") 'mc/mark-previous-like-this)
  (def-key c-quote-map (kbd "C-s") 'mc/edit-lines)

  (def-key c-quote-map (kbd "C-l") 'avy-goto-line)

  (def-key c-quote-map (kbd "C-r") 'list-registers)

  )

(c-quote-map-init)

(def-key c-quote-map (kbd "C-_") 'my-column-ruler)
(def-key c-quote-map (kbd "C--") 'dynamic-ruler)
(def-key c-quote-map (kbd "C-|") 'dynamic-ruler-vertical)


; Global Bindings Starting With C-x r:
;
;	C-@	point-to-register
;	ESC	Prefix Command
;	SPC	point-to-register
;	+	increment-register
;	N	rectangle-number-lines
;	b	bookmark-jump
;	c	clear-rectangle
;	d	delete-rectangle
;	f	frameset-to-register
;	g	insert-register
;	i	insert-register
;	j	jump-to-register
;	k	kill-rectangle
;	l	bookmark-bmenu-list
;	m	bookmark-set
;	n	number-to-register
;	o	open-rectangle
;	r	copy-rectangle-to-register
;	s	copy-to-register
;	t	string-rectangle
;	w	window-configuration-to-register
;	x	copy-to-register
;	y	yank-rectangle
;	C-SPC	point-to-register
