; problem here is that C-return is a little bit fiddly
; the expand key must be both accessible and not an otherwise
; well used key
; also if we want multiple options for snippets we might
; as well create the snippet and save it

(defhydra aya-hydra (:color pink :pre nil :post nil)
  "Aya\n"

  ("M-c" (aya-expand) "Expand")

  ("C-g" nil "quit" :exit t)
  )

(define-key global-map (kbd "A-n") 'aya-hydra/body)
