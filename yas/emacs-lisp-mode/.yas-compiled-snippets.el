;;; Compiled snippets and support files for `emacs-lisp-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'emacs-lisp-mode
		     '(("aaa" "function c_${1:test} {\n    var xmlhttp;\n    xmlhttp=bvc.XMLHttpRequest() ;\n\n    xmlhttp.local = {\"element\": \"${2:#generic-status}\"} ;\n\n    xmlhttp.onreadystatechange=show ;\n\n    xmlhttp.open(\"GET\",url_r(\"/$1\"),true);\n    xmlhttp.send();\n}\n" "aaa" nil nil nil nil "\"H-a H-z\"" nil)
		       ("pim" "${1:test} $1\n" "pim" nil nil nil nil "\"H-a H-a\"" nil)
		       ("pud" "pud!!!!!\n" "pud" nil nil nil nil "\"H-a H-s\"" nil)
		       ("top" "(setq ${1:`(thing-at-point 'symbol)`} $0) ; OK!\n${2:`(format-time-string \"field with calculated default %c\")`}\n${3:`(format \"<%s>\" (thing-at-point 'symbol))`}\n" "tap (key is top)" nil nil nil nil "\"s-/ s-.\"" nil)))


;;; Do not edit! File generated at Sat Feb 22 18:22:20 2014
