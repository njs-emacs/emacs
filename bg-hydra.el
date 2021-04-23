(defun bg-quick (c)
  (modify-frame-parameters (frame-focus) `((background-color . ,c)))
  )

(defhydra bg-hydra (:pre nil :post nil)
  "change background\n"
  ("a" (bg-quick "MistyRose") "MistyRose" :column "Nice:")
  ("d" (bg-quick "LightGray") "LightGray")

  ("g" (bg-quick "#e0f0e0") "greenish" :column "Tint:")
  ("r" (bg-quick "#f0e0e0") "reddish")
  ("b" (bg-quick "#e0e0f0") "blueish")
  
  ("3" (bg-quick "gray33") "30" :column "Gray:")
  ("4" (bg-quick "gray44") "40")		; cannot get '%' to work
  ("5" (bg-quick "gray55") "50")
  ("6" (bg-quick "gray66") "60")
  ("7" (bg-quick "gray77") "70")
  ("8" (bg-quick "gray88") "80")
  ("9" (bg-quick "gray99") "90")

  ("q" nil "quit" :column ":")
  )

(define-key global-map [M-f5] 'bg-hydra/body)

;(list-colors-display)
