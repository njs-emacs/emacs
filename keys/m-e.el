(setq m-e-map (make-sparse-keymap))

(global-set-key (kbd "M-e") m-e-map)

(def-key m-e-map (kbd "E") 'keydef-edit-init)

(def-key m-e-map (kbd "M-w") 'magit-ediff-compare)
(def-key m-e-map (kbd "M-q") 'magit-ediff-popup)
(def-key m-e-map (kbd "M-c") 'ediff-current-file)

(def-key m-e-map (kbd "M-e") 'edb)
(def-key m-e-map (kbd "M-b") 'ediff-buffers)
(def-key m-e-map (kbd "M-f") 'ediff-files)
(def-key m-e-map (kbd "M-d") 'ediff-directories)
(def-key m-e-map (kbd "M-r") 'ediff-regions-linewise)

