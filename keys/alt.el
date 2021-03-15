(def-key global-map (kbd "<A-backspace>") 'winner-undo)
(def-key global-map (kbd "<A-M-backspace>") 'winner-redo)

(def-key global-map (kbd "<A-return>") 'ace-jump-buffer)
(def-key global-map (kbd "A-#") 'ace-jump-line-mode)

(def-key global-map (kbd "A-[") 'bmkp-previous-bookmark-repeat)
(def-key global-map (kbd "A-]") 'bmkp-next-bookmark-repeat)

;; (def-key global-map (kbd "A-.") "p name\nc\n")

(def-key global-map (kbd "A-s") 'magit-stage-this-file)
(def-key global-map (kbd "A-c") 'magit-commit-create)

(def-key global-map (kbd "A-1") (ilambda (bob) (query-replace-regexp "define-key" "def-key")))

(def-key global-map (kbd "A-m") 'eval-noname)
