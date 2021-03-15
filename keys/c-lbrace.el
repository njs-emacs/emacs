(setq c-lbrace-map (make-sparse-keymap))
(global-set-key (kbd "C-{") c-lbrace-map)

(def-key c-lbrace-map (kbd "E") 'keydef-edit-init)

(def-key c-lbrace-map (kbd "SPC") 'ace-swap-window)
(def-key c-lbrace-map (kbd "C-m") 'ace-jump-line-mode)
(def-key c-lbrace-map (kbd "/") 'ace-jump-word-mode)
(def-key c-lbrace-map (kbd "k") 'ace-delete-window)

(def-key c-lbrace-map (kbd "#") 'ace-jump-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def-key c-lbrace-map (kbd "'") 'ace-jump-same-mode-buffers)

(def-key c-lbrace-map (kbd "j") 'ffx)
(def-key c-lbrace-map (kbd "n") 'ffn)

(def-key c-lbrace-map (kbd "c") 'magit-commit-create)

(def-key c-lbrace-map (kbd "C-p") 'hydra-text-scale/body)

(def-key c-lbrace-map (kbd "e") 'ediff-regions-linewise)

(def-key c-lbrace-map (kbd "<return>") 'eval-noname)
