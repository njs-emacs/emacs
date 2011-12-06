(qi-define
 (control-key-vector ?c ?h)
 '(list
   (format "#ifndef __%s_h\n#define __%s_h\n\n" (basename) (basename))
   "\n#endif\n")
 )

(defun insert-protected-header (s)
  (interactive "sHeader Basename: ")
  (insert (format "#ifndef __%s_h\n#include\t\"%s.h\"\n#endif\n" s s))
  )

(global-set-key (control-key-vector ?o ?c ?p) 'insert-protected-header)
