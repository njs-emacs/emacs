(defun my-reg-init ()
  (setq my-reg-keymap (copy-keymap ctl-x-r-map))
  (define-key my-reg-keymap (kbd "C-k") 'kmacro-to-register)
  (define-key global-map (kbd "C-'") my-reg-keymap)
  (define-key my-reg-keymap (kbd "C-'") 'bookmark-jump)
  (define-key my-reg-keymap (kbd "C-;") 'bookmark-set)
  (define-key my-reg-keymap (kbd "C-/") 'bookmark-bmenu-list)

  (define-key my-reg-keymap (kbd "C-#") 'ace-jump-line-mode)
  (define-key my-reg-keymap (kbd "C-.") 'ace-jump-buffer)
  (define-key my-reg-keymap (kbd "C-=") 'ace-window)
  (define-key my-reg-keymap (kbd "C-w") 'ace-jump-word-mode)

  (define-key my-reg-keymap (kbd "C-j") 'jump-to-register)
  
  (define-key my-reg-keymap (kbd "C-b") 'recentf-open-files)
  (define-key my-reg-keymap (kbd "C-h") nil)
  (define-key my-reg-keymap (kbd "C-c") 'command-history)

  (define-key my-reg-keymap (kbd "C-n") 'mc/mark-next-like-this)
  (define-key my-reg-keymap (kbd "C-p") 'mc/mark-previous-like-this)
  (define-key my-reg-keymap (kbd "C-s") 'mc/edit-lines)
  )

(my-reg-init)

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
