(setq c-dollar-map (make-sparse-keymap))
(global-set-key (kbd "C-$") c-dollar-map)

(def-key-map "C-$" 'c-dollar-map)

;(def-key c-dollar-map (kbd "E") 'keydef-edit-init)

;(def-key c-dollar-map (kbd "<home>") 'helm-buffers-list)

(def-key c-dollar-map (kps "1") 'helm-buffers-list)
(def-key c-dollar-map (kps "4") 'helm-find-files)

(def-key c-dollar-map (kps "55") 'hydra-superhelp/body)
