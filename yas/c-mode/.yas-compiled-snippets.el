;;; Compiled snippets and support files for `c-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'c-mode
		     '(("inc" "#ifndef __${1:foo}\n#define __$1\n\n#endif\n" "inc" nil nil nil nil "\"H-a H-x\"" nil)
		       ("poop" "#ifndef __${1:foo}\n#define __$1\n\n#endif\n" "poop" nil nil nil nil nil nil)
		       (nil "#ifndef __$1\n#include	\"$1.h\"\n#endif\n" "tug" nil nil nil nil "\"H-a H-s\"" nil)))


;;; Do not edit! File generated at Sat Feb 22 18:22:20 2014
