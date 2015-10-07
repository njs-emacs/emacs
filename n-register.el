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

  (define-key my-reg-keymap (kbd "C-b") 'recentf-open-files)
  
  )

(my-reg-init)

