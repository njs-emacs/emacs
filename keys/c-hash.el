(setq c-hash-map (make-sparse-keymap))
(global-set-key (kbd "C-#") c-hash-map)

(def-key-map "C-#" 'c-hash-map)

(def-key c-hash-map (kbd "SPC") 'mark-word)
(def-key c-hash-map (kbd "C-SPC") 'mark-sexp)
(def-key c-hash-map (kbd "C-s") 'magit-update-index)
(def-key c-hash-map (kbd "C-d") 'pending-delete-mode)

(def-key c-hash-map [C-home] 'beginning-of-defun)
(def-key c-hash-map [C-end] 'end-of-defun)

(def-key c-hash-map (kbd "C-#") 'avy-goto-line)
(def-key c-hash-map (kbd "C-.") 'avy-goto-word-or-subword-1)

(def-key c-hash-map (kbd "C-f") 'find-file-at-point)

(def-key c-hash-map "\C-i" 'insert-register)

(def-key c-hash-map "\C-c" 'copy-to-register)

(def-key c-hash-map (kbd "C-'") 'avy-bookmark)

;(def-key c-hash-map (kbd "C-e C-e") 'eval-buffer-forms-ediff-enter)
;(def-key c-hash-map (kbd "C-e C-x") 'eval-buffer-forms-ediff-leave)
  
;(def-key c-hash-map (kbd "C-e C-c") 'lo-to-c-symbol-replace)
;;

