(setq growl-path "D:/G/Growl")
(setq growl-exe (filename-concat growl-path "growl.exe"))
(setq growl-notify-path (filename-concat growl-path "growlnotify.exe"))

(defun growl-start ()
  (interactive)
  (start-process "growl-main" " growl-main" growl-exe)
  )

(defun growl-notify (cmd)
  (let* ((cmd (format "%s %s" growl-notify-path cmd)))
    (print cmd)
    (shell-execute-text cmd)
    )
  )

(defun growl (&rest plist)
  (let* ((title (plist-get plist :title))
	 (message (plist-get plist :message))
	 (app (plist-get plist :app))
	 (types (plist-get plist :types))
	 (type (plist-get plist :type))
	 (raw (plist-get plist :raw))
	 args
	 )
    (and raw (push raw args))
    (cond
     (app (push (format "\"/a:%s\"" app) args)))
    (cond
     (types (push (format "\"/r:%s\"" types) args)))
    (cond
     (type (push (format "\"/n:%s\"" type) args))
     )
    (cond
     (message
      (and title (push (format "\"/t:%s\"" title) args))
      (push (format "\"%s\"" message) args)
      )
     )
    (setq cmd (mconcat (nreverse args) " "))
    (growl-notify cmd)
    )
  )

;=; (top-level)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;#; (growl-start)

;#+; (growl-notify "\"/a:snap\" \"/n:fart1\" /t:yo mama")
;#+; (growl :raw "\"/a:snap\" \"/n:fart1\" /t:yo mama")

;#+; (growl :app "snap" :type "fart1" :message "ma!" :title "yo mama")

;#+; (growl :raw "\"/a:snap\" /t:yo" :type "fart1" :message "hello")
;#+; (growl :raw "\"/a:snap\" /t:yo" :type "fart1" :message "ma!")
;#+; (growl :raw "\"/a:snap\" \"/t:yo mama\"" :type "fart1" :message "ma!")

;#+; (growl :raw "\"/a:snap\"" :type "fart1" :message "ma!" :title "yo mama")

;#!; (growl :app "snap" :type "fart1" :title "amoop" :message "mama mia")
;# ; (growl :app "snap" :type "fart1" :title "amoop" :message "\"mama mia\"")
;#+; (growl :app "snap" :type "fart1" :title "amoop" :message "mama")
;#!; (growl :app "snap" :type "fart1" :title "yo mama" :message "mama mia")
;#!; (growl :app "snap" :type "fart1" :message "mama mia" :title "yo mama")

;#; (growl :app "snat" :types "fartA,fartB,fartC" :type "fartC" :message "ma!" :title "yo mama")
;#; (growl :app "snat" :type "fartA" :message "ma!" :title "yo mama")
;#; (growl :app "snat" :type "fartC" :message "ma!" :title "yo mama")

;#; (growl :raw "\"/a:snap\"" :title "yoohoo" :type "fart1" :message "hello world")

;#; (growl :app "snap" :type "fart1" :title "amoop" :message "mama mia")
;#; (growl :app "snax" :types "fart1,fart2,fart3" :type "fart1" :title "amoop" :message "mama mia")
;#; (growl :app "snax" :type "fart1" :message "mama mia")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun growl-a ())
(setq grat (run-at-time (time-add nil 30) nil 'growl-a))


