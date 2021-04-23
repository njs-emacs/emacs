(setq c-comma-map (make-sparse-keymap))
(global-set-key (kbd "C-,") c-comma-map)

(def-key-map "C-," 'c-comma-map)

(def-key c-comma-map (kbd "E") 'keydef-edit-init)

(def-key c-comma-map (kbd "C-.") 'mc/mark-more-like-this-extended)
(def-key c-comma-map (kbd "C-x") 'mc/mark-all-like-this)
(def-key c-comma-map (kbd "C-SPC") 'mc/edit-lines)
(def-key c-comma-map (kbd "C-a") 'mc/edit-beginnings-of-lines)
(def-key c-comma-map (kbd "C-e") 'mc/edit-ends-of-lines)

;(def-key c-comma-map (kbd "C-s") 'mc/mark-all-in-region)
(def-key c-comma-map (kbd "C-s") 'mc/mark-all-in-region-regexp)

(def-key c-comma-map (kbd "C-n") 'mc/mark-next-like-this)
(def-key c-comma-map (kbd "C-p") 'mc/mark-previous-symbol-like-this)
(def-key c-comma-map (kbd "C-,") 'mc/mark-all-dwim)

(def-key c-comma-map (kbd "C-o") 'mc--mark-symbol-at-point)

