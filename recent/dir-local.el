(defvar local-dir-foo t "Local dir variable")
(defvar local-dir-yab t "Local dir variable - yab")

(put 'local-dir-yab 'safe-local-variable #'stringp)

