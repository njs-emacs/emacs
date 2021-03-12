(setq c-v-map (make-sparse-keymap))
(global-set-key (kbd "C-v") c-v-map)

(def-key c-v-map (kbd "E") 'keydef-edit-init)

; [[file:e:/.p/.emacs/extra.el::127][extra.el]]

(def-key c-v-map (kbd "C-#") 'enquote-at-point-d)

(def-key c-v-map (kbd "C-'") 'enquote-at-point-s)
(def-key c-v-map (kbd "C-q") 'enquote-at-point-d)

(def-key c-v-map (kbd "C-s") 'enquote-at-point-s)
(def-key c-v-map (kbd "C-v") 'scroll-up)

