;;; Compiled snippets and support files for `text-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'text-mode
		     '(("aaa" "function c_${1:test} {\n    var xmlhttp;\n    xmlhttp=bvc.XMLHttpRequest() ;\n\n    xmlhttp.local = {\"element\": \"${1:#generic-status}\"} ;\n\n    xmlhttp.onreadystatechange=show ;\n\n    xmlhttp.open(\"GET\",url_r(\"/$1\"),true);\n    xmlhttp.send();\n}\n" "aaa" nil nil nil nil "\"H-a H-z\"" nil)))


;;; Do not edit! File generated at Sat Feb 22 18:22:20 2014
