(setq key-chord-two-keys-delay .1)
(setq key-chord-one-key-delay .5)

; keys that don't usually appear doubled

; (dolist (k '(q w y u i j h k v))
; (dolist (k '(u i j h))

(dolist (k '(u i j h))
  (let* ((map-sym (intern (format "key-chord-%s-map" k)))
	 (map (make-sparse-keymap))
	 )
    (key-chord-define-global (format "%s%s" k k) map)
    (set map-sym map)
    (define-key map (kbd "TAB")
      `(lambda () (interactive) (which-key-show-full-keymap ',map-sym)))
    )
  )

(define-key key-chord-u-map (kbd "t") 'symbol-overlay-hydra/body)

(define-key key-chord-h-map (kbd "k") 'describe-key)
(define-key key-chord-h-map (kbd "a") 'apropos)
(define-key key-chord-h-map (kbd "v") 'describe-variable)
(define-key key-chord-h-map (kbd "f") 'describe-function)

(define-key key-chord-j-map "j" 'find-file)

(top-level)
(def-key global-map "
;;;; below here obsolete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun kc-map-help () (interactive) (which-key-show-full-keymap 'kc-map))

(setq kc-map (make-sparse-keymap))

(define-key kc-map (kbd "TAB") 'kc-map-help)

(which-key-mode)
(setq which-key-show-early-on-C-h t)
(setq which-key-idle-delay 2.0)
(setq which-key-idle-secondary-delay 0.05)

(dolist (binding
         `(("ii" . ,kc-map)
	   (" i" . previous-multiframe-window)
           (" o" . next-multiframe-window)
           (" l" . ibuffer)
           
           (" m" . magit)

           (" e" . er/expand-region)
           
           (" q" . quake-mode)
           
           (" 0" . delete-window)
           (" 1" . delete-other-windows)
           (" 2" . split-window-below)
           (" 3" . split-window-right)
           (" =" . winstack-push)
           (" -" . winstack-pop)
           
           (" w" . whitespace-mode)
           
           ("ji" . undo-tree-undo)
           ("jo" . undo-tree-redo)
           ("jk" . undo-tree-switch-branch)
           ("j;" . undo-tree-visualize)
           
           (" b" . ido-switch-buffer)
           (" f" . ido-find-file)
           (" s" . save-buffer)
           
           (" x" . shell)
           
           (" \\". jorbi/toggle-comment)
           
           ("nw" . jabber-display-roster)
           ("ne" . jabber-chat-with)
           
           ("nv" . jorbi/find-init-file)
           
           (" r" . recompile)))
;  (key-chord-define-local (car binding) (cdr binding))
  (key-chord-define-local (car binding) nil)

)



(dolist (binding
         `(("ii" . ,kc-map)
	   ))
  (key-chord-define-local (car binding) (cdr binding))
;  (key-chord-define-local (car binding) nil)
)




