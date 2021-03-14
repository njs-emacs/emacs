; list
(defun pst (&optional arg) "Keyboard macro."
  (interactive "p")
  (kmacro-exec-ring-item (list (kbd "C-x 2") 0 "%d") arg)
  )

; quote
(defun pst (&optional arg) "Keyboard macro."
  (interactive "p")
  (kmacro-exec-ring-item '("\C-x2" 0 "%d") arg)
  )

; bquote
(defun pst (&optional arg) "Keyboard macro."
  (interactive "p")
;  (kmacro-exec-ring-item `(,(kbd "<f3>") 0 "%d") arg)
;  (kmacro-exec-ring-item `(,(kbd "<f3>") nil "%4d") arg)
  (kmacro-exec-ring-item `(,(kbd "<f3>")) arg)
  )

(describe-key (kbd "<f4>"))
kmacro-end-or-call-macro



(defmacro kmacro-fn (keys)
  `(lambda (&optional arg) "" (interactive "p")
     (kmacro-exec-ring-item `(,(kbd keys)) arg)))

(fset 'foo [return 3 16 14 5 return 21 3 46 return 24 98 return])
(setq foo-keys [return 3 16 14 5 return 21 3 46 return 24 98 return])
(edit-kbd-macro foo-keys)






kmacro

(kmacro-bind-to-key (kbd "C-! C-p"))

;(define-key c-bang-map (kbd "w") (kmacro-fn "[ ]"))

(kmacro-fn "[ f3 ]")

;(define-key c-bang-map (kbd "w") 'pst)

^X2^X2^X2^X2

foo 0 poo
kmacro-call-macro

(key-description (kbd "C-a C-e"))


