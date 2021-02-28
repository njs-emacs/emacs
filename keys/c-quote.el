(defun c-quote-map-init ()
  (setq c-quote-map (copy-keymap ctl-x-r-map))

  (define-key c-quote-map (kbd "E") 'keydef-edit-init)

  (define-key global-map (kbd "C-'") c-quote-map)

  (define-key c-quote-map (kbd "C-k") 'kmacro-to-register)
  (define-key c-quote-map (kbd "C-'") 'bookmark-jump)
  (define-key c-quote-map (kbd "C-;") 'bookmark-set)
  (define-key c-quote-map (kbd "C-/") 'bookmark-bmenu-list)

  (define-key c-quote-map (kbd "C-#") 'ace-jump-line-mode)
  (define-key c-quote-map (kbd "C-.") 'ace-jump-buffer)
  (define-key c-quote-map (kbd "C-=") 'ace-window)
  (define-key c-quote-map (kbd "C-w") 'ace-jump-word-mode)

  (define-key c-quote-map (kbd "C-j") 'jump-to-register)
  
  (define-key c-quote-map (kbd "C-b") 'recentf-open-files)
  (define-key c-quote-map (kbd "C-c") 'command-history)

  (define-key c-quote-map (kbd "C-n") 'mc/mark-next-like-this)
  (define-key c-quote-map (kbd "C-p") 'mc/mark-previous-like-this)
  (define-key c-quote-map (kbd "C-s") 'mc/edit-lines)

  (define-key c-quote-map (kbd "C-l") 'avy-goto-line)

  (define-key c-quote-map (kbd "C-r") 'list-registers)

  )

(c-quote-map-init)

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
