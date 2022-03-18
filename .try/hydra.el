(defun ts (&optional time) (format-time-string "%c" time))
(setq hint-test-state nil)
(defhydra hint-test
 (
  :pre (message "pre")
  :post (message "post")
  :hint nil
  :base-map superhelp-base-map
  :timeout 10
)
"
_i_:   Insert %`hint-test-state %(ts)
"
  ("i" (flip hint-test-state) :color pink)
  ("q" nil :color blue)
  )

(define-key global-map (kbd "H-x H-1") 'hint-test/body)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-hydra-1 ()
  (interactive)
  (eval
   `(defhydra my-hydra-1 (:color blue) "My hydra"
      ,(if (evenp (line-number-at-pos))
           '("e" (message-box "Even line") "Even")
         '("o" (message-box "Odd line") "Odd"))
      ,(when t '("a" (message-box "always true") "always"))
      ;; This does not work. you must return a legitimate hydra menu item
      ;;      ,(when nil '("n" (message-box "never") "never"))
      ))
  (my-hydra-1/body))

(my-hydra-1)
(my-hydra-1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-hydra-2 ()
  (interactive)
  (let ((conditionals '((if (evenp (line-number-at-pos))
                            '("e" (message-box "Even second") "Even")
                          '("o" (message-box "Odd second") "Odd"))
                        (when t '("a" (message-box "always true") "always"))
                        (when nil '("n" (message-box "never") "never")))))
    (eval
     (append
      '(defhydra my-hydra-2 (:color blue) "My hydra")
      (loop for cond in conditionals
            with result = (eval cond)
            if (eval cond)
            collect (eval cond))))
    (my-hydra-2/body)))

(my-hydra-2)
(my-hydra-2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-hydra-3 ()
  (interactive)
  (let ((conditionals
         `(((evenp (line-number-at-pos)) . ("e" (message-box "Even second") ,(format "Even: %s" (line-number-at-pos))))
           ((oddp (line-number-at-pos)) . ("o" (message-box "Odd second") ,(format "Odd: %s" (line-number-at-pos))))
           (t . ("a" (message-box "always true") "always"))
           (nil . ("n" (message-box "never") "never")))))
    (eval
     (append
      '(defhydra my-hydra-3 (:color blue) "My hydra")
      (loop for cond in conditionals
            if (eval (car  cond))
            collect (cdr cond))))
    (my-hydra-3/body)))

(my-hydra-3)
(my-hydra-3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-hydra-4 ()
  (interactive)
  (let ((conditionals
         `(((evenp (line-number-at-pos)) . ("e" (message-box "Even second") ,(format "Even: %s" (line-number-at-pos))))
           ((oddp (line-number-at-pos)) . ("o" (message-box "Odd second") ,(format "Odd: %s" (line-number-at-pos))))
           (t . ("q" nil "quit"))
           (t . ("g" (message-box "go on...") "go on..." :color pink))
           (t . ("a" (message-box "always true") "always"))
           (nil . ("n" (message-box "never") "never")))))
    (eval
     (append
      '(defhydra my-hydra-4 (:color pink) "My hydra")
      (loop for cond in conditionals
            if (eval (car  cond))
            collect (cdr cond))))
    (my-hydra-4/body)))

(my-hydra-4)
(my-hydra-4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; this one doesn't have a :pre 
: the caption gets refreshed on return from message-box
(defun my-hydra-5 ()
  (interactive)
  (let ((conditionals
         `(
	   ((evenp (line-number-at-pos)) . ("e" (message-box "Even second") ,(format "Even: %s" (line-number-at-pos))))
           ((oddp (line-number-at-pos)) . ("o" (message-box "Odd second") ,(format "Odd: %s" (line-number-at-pos))))
           (t . ("q" nil "quit"))
           (t . ("g" (message-box "go on...") "go on..." :color pink))
           (t . ("a" (message-box "always true") "always"))
           (nil . ("n" (message-box "never") "never")))))
    (eval
     (append
      '(defhydra my-hydra-5 (:color pink :hint nil)
"
_g_:   Insert %(il)
")


      (loop for cond in conditionals
            if (eval (car  cond))
            collect (cdr cond))))
    (my-hydra-5/body)))

(my-hydra-5)
(my-hydra-5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; this one doesn't have a :pre 
: the caption gets refreshed on return from message-box
;
; foreign keys don't trigger hint repaint (can this be forced?)
;

(defun pass-through-key ()
  (interactive)
  (let* ((cmd (this-command-keys))
	 (maps (current-active-maps))
	 (binding (key-binding cmd))
	 )
    (call-interactively binding)
;    (debug)
    )
  )

(defun my-hydra-5 ()
  (interactive)
  (let ((conditionals
         `(
	   ((evenp (line-number-at-pos)) . ("e" (message-box "Even second") ,(format "Even: %s" (line-number-at-pos))))
           ((oddp (line-number-at-pos)) . ("o" (message-box "Odd second") ,(format "Odd: %s" (line-number-at-pos))))
           (t . ("q" nil "quit"))
           (t . ("g" (message-box "go on...") "go on..." :color pink))
           (t . ("a" (message-box "always true") "always"))
	   (t . ("<down>" pass-through-key "down" :exit nil))
	   (t . ("<up>" pass-through-key "up" :exit nil))
           (nil . ("n" (message-box "never") "never")))))
    (eval
     (append
      '(defhydra my-hydra-5 (:color pink :hint nil)
"
_g_:   Insert %(il)
_q_:   Quit
")


      (loop for cond in conditionals
            if (eval (car  cond))
            collect (cdr cond))))
    (my-hydra-5/body)))

(my-hydra-5)
(my-hydra-5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; :bind is used as a method to bind the head, it's not the key itself

; any heads that have hints (a string following the command) will appear
; in the hints.
; if the main hydra property :hin is not nil 
; default hints are generated from the heads

(defhydra my-hydra-5x (:color pink)
  "
_g_:   Insert %(il)
_q_:   Quit
"
  ("q" nil)
  ("g" (message-box "go on...") "FOO!" :color pink)
  ("a" (message-box "always true"))
  ("<down>" pass-through-key :exit nil :hint nil)
  ("<up>" pass-through-key :exit nil :hint nil)
)
(my-hydra-5x/body)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defhydra my-hydra-5y (:color pink :hint nil)
  "
_i_:   Insert %(il)
_q_:   Quit
"
  ("i" (insert (il)))
  ("q" nil)
  ("<down>" pass-through-key :exit nil :hint nil)
  ("<up>" pass-through-key :exit nil :hint nil)
)
(my-hydra-5y/body)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-face-foreground 'hydra-face-pink "DeepPink")
(set-face-foreground 'hydra-face-pink "maroon")

; this research can be useful for building hydras with smart hints
; and conditional content.
; it would appear to be difficult if not impossible to tailor the
; hydra content between keystrokes other than to modify hints
; although it would be possible for a hydra to define a second
; hydra and then invoke it, but that is bound to be limited in the
; long run due to deep nesting.
