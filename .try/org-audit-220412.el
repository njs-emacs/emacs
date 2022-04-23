(org-capture-template-add "a"
 '("audit" entry (file+headline "" "Audit") "* %? %^g"))
 ("todo" entry (file+headline "" "Tasks") "* TODO %?\n  %T"))


table-line

(org-capture-template-add "L"
  '("line" table-line (file+olp "" "this") "| %? %U | %A |" :table-line-pos "II-2"))

(org-capture-template-add "Q" `("prefix"))
(org-capture-template-add "QA"
  '("line" table-line (file+olp "" "this") "| QA | %U |" :table-line-pos "I+2"))

(org-capture-template-add "QA"
  '("line" table-line (file+olp "" "this") "| QA | %T |" :table-line-pos "I-2"))

(org-capture-template-add "QW"
  '("line" table-line (file+olp "" "this") "| QW | %l |" :prepend t :table-line-pos "II+2"))

(org-capture-template-add "Z"
  '("line" table-line (file+olp "" "this") "| %^L | %l |" :prepend t :table-line-pos "II+2"))

(define-key global-map (kbd "A-.") 'org-capture)

(org-capture-template-remove "L")
(org-capture-template-remove "QA")
(org-capture-template-remove "QW")
(org-capture-template-remove "Q")
