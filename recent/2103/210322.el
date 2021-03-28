(defun expand-file-names (&rest list)
  (mapcar 'expand-file-name (flatten list))
  )

