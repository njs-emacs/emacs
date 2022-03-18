(use-package org-alert
  :ensure t
  :init
  :custom
     (alert-default-style 'notification)
     (org-alert-interval 60)
  :config
;  (org-alert-enable)
  (setq org-alert-interval 60)
  )

;org-alert
;(org-alert-enable)
;(org-alert-disable)
; (setq alert-default-style 'message)
; (setq alert-default-style 'notification)
; (setq alert-default-style 'momentary)
; (setq alert-default-style 'w32-notification-notify)
; (setq alert-default-style 'notifier)
; (setq alert-default-style 'mode-line)
; (setq alert-default-style 'toaster)
(alert "This is an alert")

(w32-notification-notify :tip "look!" :level "warning" :title "Emacs says..."
			 "The following text...")

(setq zz
  (w32-notification-notify
   :icon nil
   :tip "look!"
;   :level "warning"
   :title "Emacs says..."
   :body (concat "The following text...\nLook at how we can blah de blah
and 
put text in a windows notification" (format-time-string " %H:%M:%S"))
   )
  )

(w32-notification-close zz)

(w32-notification-close 40)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(top-level)

(setq grate (run-at-time "16:50" nil 'growl-a))

(defun growl-compilation-result(buffer msg)
  (if (string-match "^finished" msg)
    (progn
     (growl "Emacs compilation" "Compilation Successful :-)"))
    (growl "Emacs compilation" "Compilation Failed :-(")))

(add-to-list 'compilation-finish-functions 'growl-compilation-result)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dregf "growl" nil "month")
