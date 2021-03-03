(defhydra text-scale-hydra (:timeout 10)
  "Scale text size"

  ("<up>" text-scale-increase "Up" :column "Big")
  ("b" text-scale-increase "Bigger")
  ("l" text-scale-increase "Larger")
  ("x" text-scale-increase "Expand")
  ("g" text-scale-increase "Grow")
  ("o" text-scale-increase "Out")
  (">" text-scale-increase "More")
  ("u" text-scale-increase "Up")

  ("<down>" text-scale-decrease "Down" :column "Small")
  ("s" text-scale-decrease "Smaller")
  ("i" text-scale-decrease "In")
  ("<" text-scale-decrease "Less")
  ("c" text-scale-decrease "Contract")
  ("d" text-scale-decrease "Down")

  ("q" nil "Quit" :exit t :column "Other"))

;; (define-key c-lt-map (kbd "s") 'text-scale-hydra/body)

