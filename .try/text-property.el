(put-text-property (point^) (point$) 'modification-hooks `(foo-foo))

x
(put-text-property (point^) (point$) 'modification-hooks `(foo-foo))

(put-text-property (point^) (point$) 'modification-hooks nil)

(put-text-property (point^) (point$) 'point-entered '(foo-foo))

(put-text-property (point^) (point$) 'point-leave '(foo-fox))

(put-text-property (point^) (point$) 'display `((margin right-margin) "splurk"))
(put-text-property (point^) (point$) 'face 'link)
(put-text-property (point^) (point$) 'face nil)
(put-text-property (point^) (point$) 'm odification-hooks `((foo-foo)))
(put-text-property (point^) (point$) 'm odification-hooks `((foo-foo)))

(defun foo-foo (&optional a b) (debug))
(defun foo-fox (&optional a b) (debug))


