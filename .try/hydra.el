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
           (t . ("q" nil "quit"))
           (t . ("g" (message-box "go on...") "go on..." :color pink))
           (t . ("a" (message-box "always true") "always"))
           (nil . ("n" (message-box "never") "never")))))
    (eval
     (append
      '(defhydra my-hydra-3 (:color pink) "My hydra")
      (loop for cond in conditionals
            if (eval (car  cond))
            collect (cdr cond))))
    (my-hydra-3/body)))

(my-hydra-3)
(my-hydra-3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
