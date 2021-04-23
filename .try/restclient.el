(defun rest-hook-nuts ()
  (let ((buffer (current-buffer)))
    (write-region
     (point-min) (point-max)
     (daily-date-path (format-time-string ".rest/%y%m%d-%H%M%S") nil nil t)
     t)
     )
  )

(add-hook 'restclient-response-loaded-hook 'rest-hook-nuts)

(url-encode-url "where={\"objectId\": \"k8i7HSjQOr\"}")
