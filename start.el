; this file is not loaded
; select the current environment

(frame-default-height)

(setq p-home (// "boo/e/.p"))
(defun p-path (path) (filename-concat p-home path))

(load-file (// "boo/e/mew/.emacs.el"))

(cons-load-path (p-path ".emacs"))

(init-local (p-path "stub"))
(top-level)
(init-local "e:/ca")
(init-local "e:/ca/htdocs/help")
(init-local "e:/.d/l/lily")
(init-local "e:/.d/s/svg")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(find-file "e:/daily/21/2103/2103.org")
(find-file "e:/daily/21/2102/2102.org")
(find-file "e:/daily/21/2101/2101.org")

(init-local "e:/ca/htdocs/pop")

(init-local "e:/.d/j/jquery")

(init-local "e:/ca")
(init-local "f:/_media/.meta/150812")
(init-local "e:/#/avisynth")
(init-local (p-path  "apihook"))
(init-local (p-path "hook/cbt"))
(init-local (p-path "hook/msg"))


(init-local (p-path "aft"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(find-file "e:/aa")
;(find-file (daily-path ".emacs.el"))
;(find-file-other-window "e:/drs/.emacs.el")
;(find-file (daily-path "1201/.month/.emacs.el"))
;(find-file "e:/daily/1209/25/proxy")
;(find-file "e:/_/L/lily")
;(find-file "e:/_/R/R")

;(find-file (// "boo/e/emacs/yas.el"))
;(find-file (// "boo/e/.p/tc"))
;(find-file (// "boo/e/.p/nl"))
;(find-file (// "boo/e/.p/stub/.emacs.el"))
;(find-file (// "boo/e/.p/android/.meta/.emacs.el"))

(top-level)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(find-file "e:/help")
(find-file "e:/help/win7")
(find-file "e:/daily/1212/pi/.emacs.el")

(find-file "f:/a/apache/spix/.emacs.el")
(find-file (concat user-emacs-home "/yas.el"))

(find-file (daily-path ".emacs.el"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(find-file (daily-path "1112/.month/.emacs.el"))
(find-file (daily-path "1111/.month/.emacs.el"))
(find-file (daily-path "1110/.month/.emacs.el"))
(find-file (daily-path "1109/.month/.emacs.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(find-file (daily-path "1004/nose/.emacs.el"))
(find-file (daily-path "1005/ovaltine/.emacs.el"))

(find-file (daily-path "1212/pi/.emacs.el"))

(find-file "e:/perl/registry/a.pl")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(alist-put default-frame-alist 'background-color "MistyRose")
(alist-put default-frame-alist 'background-color "#e0d0d0")

(modify-frame-parameters (frame-focus) '((background-color . "MistyRose")))
(modify-frame-parameters (frame-focus) '((background-color . "#e0d0d0")))
(modify-frame-parameters (frame-focus) '((background-color . "#d0f0d0")))

zonk
2r

(abbrev-symbol "wpa" zonk-abbrev)
(abbrev-expansion "wpa" zonk-abbrev)
(abbrev-insert 'wpa)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-faces
 '(outline-1 ((((background light)) (:foreground "Blue"))))
; '(outline-2 ((((background light)) (:foreground "DodgerBlue"))))
 '(outline-2 (
	      (((background light)) (:foreground "DarkGreen")))
	     )
 '(outline-3 (
;	      (((background light)) (:foreground "DarkViolet")))
	      (((background light)) (:foreground "Cyan4")))
	     )
 )

(set-face-background 'default "#CCCCCC" (window-frame (frame-selected-window)))
(set-face-background 'default "#dddddd" (window-frame (frame-selected-window)))
(set-face-background 'default "#eeeeee" (window-frame (frame-selected-window)))
