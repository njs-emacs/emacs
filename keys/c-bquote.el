(setq c-bquote-map (make-sparse-keymap))
(global-set-key (kbd "C-`") c-bquote-map)

(def-key-map "C-`" 'c-bquote-map)

(def-key c-bquote-map (kbd "E") 'keydef-edit-init)

(def-key c-bquote-map (kbd "C-c") 'avy-copy-region)

(def-key c-bquote-map (kbd "C-k C-k") 'avy-kill-whole-line)

(def-key c-bquote-map (kbd "C-x") 'avy-kill-region)

(def-key c-bquote-map (kbd "C-p") 'avy-copy-line)
(def-key c-bquote-map (kbd "C-m") 'avy-move-region)
(def-key c-bquote-map (kbd "C-l") 'avy-move-line)

(def-key c-bquote-map (kbd "C-e") 'avy-goto-end-of-line)
(def-key c-bquote-map (kbd "C-a") 'avy-goto-line)

(def-key c-bquote-map (kbd "C-s") 'avy-goto-symbol-1)
(def-key c-bquote-map (kbd "C-` C-s") 'avy-copy-symbol-1)
(def-key c-bquote-map (kbd "C-` C-` C-s") 'avy-copy-symbol-1-other-window)

(def-key c-bquote-map (kbd "C-q") 'avy-goto-word-this-line)
(def-key c-bquote-map (kbd "C-w") 'avy-goto-word-1)
(def-key c-bquote-map (kbd "C-r") 'avy-goto-char-2)


(def-key c-bquote-map (kbd "C-[") 'avy-sexp-copy)
