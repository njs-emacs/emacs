; collection of all key-translation-map bindings we want to disable
; so that we can use the ALT key modifier effectively

; look here -> d:/E/emacs-25/share/emacs/25.1/lisp/bindings.el::1070

;(describe-variable 'key-translation-map)
;(lookup-key key-translation-map (kbd "A-~"))

;; (define-key key-translation-map  (kbd "A-m") nil)

(define-key key-translation-map (kbd "C-x 8") nil)

; find out which codes ALT modified keys generate
;(kbd "A-SPC")
;(kbd "A-!")
;(kbd "A-A")
;(kbd "A-a")

(global-unset-key (kbd "A-m"))

(let ((base (aref (kbd "A-SPC") 0)))
	    
  (dotimes (i 16)
    (define-key key-translation-map (vector (+ base i 0)) nil) ; this was commented out - don't know why
    (define-key key-translation-map (vector (+ base i 16)) nil)
    (define-key key-translation-map (vector (+ base i 32)) nil)
    )

  (dotimes (i 26)
    (define-key key-translation-map (vector (+ base i 32)) nil)
    (define-key key-translation-map (vector (+ base i 64)) nil)
    )
  )

(define-key key-translation-map  (kbd "A-^") nil)
(define-key key-translation-map  (kbd "A-~") nil)
(define-key key-translation-map  (kbd "A-`") nil)
(define-key key-translation-map  (kbd "A-a") nil)
(define-key key-translation-map  (kbd "A-_") nil)
(define-key key-translation-map  (kbd "A-]") nil)
(define-key key-translation-map  (kbd "A-[") nil)
(define-key key-translation-map  (kbd "A-}") nil)
(define-key key-translation-map  (kbd "A-{") nil)
(define-key key-translation-map  (kbd "A-|") nil)

(define-key key-translation-map (kbd "<S-dead-acute>") nil)
(define-key key-translation-map (kbd "<S-dead-asciicircum>") nil)
(define-key key-translation-map (kbd "<S-dead-asciitilde>") nil)
(define-key key-translation-map (kbd "<S-dead-circum>") nil)
(define-key key-translation-map (kbd "<S-dead-circumflex>") nil)
(define-key key-translation-map (kbd "<S-dead-diaeresis>") nil)
(define-key key-translation-map (kbd "<S-dead-grave>") nil)
(define-key key-translation-map (kbd "<S-dead-tilde>") nil)
(define-key key-translation-map (kbd "<dead-acute>") nil)
(define-key key-translation-map (kbd "<dead-asciicircum>") nil)
(define-key key-translation-map (kbd "<dead-asciitilde>") nil)
(define-key key-translation-map (kbd "<dead-circum>") nil)
(define-key key-translation-map (kbd "<dead-circumflex>") nil)
(define-key key-translation-map (kbd "<dead-diaeresis>") nil)
(define-key key-translation-map (kbd "<dead-grave>") nil)
(define-key key-translation-map (kbd "<dead-tilde>") nil)
(define-key key-translation-map (kbd "<mute-acute>") nil)
(define-key key-translation-map (kbd "<mute-asciicircum>") nil)
(define-key key-translation-map (kbd "<mute-asciitilde>") nil)
(define-key key-translation-map (kbd "<mute-diaeresis>") nil)
(define-key key-translation-map (kbd "<mute-grave>") nil)

(define-key key-translation-map (kbd "<mute-grave>") nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; don't do this...
; (define-key key-translation-map (kbd "<kp-1>") (kbd "C-~"))

; do this

(define-key function-key-map (kbd "<kp-divide>") (kbd "C-/"))
(define-key function-key-map (kbd "<kp-multiply>") (kbd "C-*"))
(define-key function-key-map (kbd "<kp-subtract>") (kbd "C-_"))
(define-key function-key-map (kbd "<kp-add>") (kbd "C-+"))
(define-key function-key-map (kbd "<kp-decimal>") (kbd "C-^"))
(define-key function-key-map (kbd "<kp-7>") (kbd "C-{"))
(define-key function-key-map (kbd "<kp-8>") (kbd "C-}"))
(define-key function-key-map (kbd "<kp-9>") (kbd "C-!"))
(define-key function-key-map (kbd "<kp-4>") (kbd "C-:"))
(define-key function-key-map (kbd "<kp-5>") (kbd "C-$"))
(define-key function-key-map (kbd "<kp-6>") (kbd "C-~"))
(define-key function-key-map (kbd "<kp-1>") (kbd "C-<"))
(define-key function-key-map (kbd "<kp-2>") (kbd "C->"))
(define-key function-key-map (kbd "<kp-3>") (kbd "C-?"))
(define-key function-key-map (kbd "<kp-0>") (kbd "C-h"))

(define-key function-key-map (kbd "<kp-enter>") (kbd "RET"))

(setq function-key-to-kp-alist
  `(
    ( ?7 . "C-{")
    ( ?8 . "C-}")
    ( ?9 . "C-!")
    ( ?4 . "C-:")
    ( ?5 . "C-$")
    ( ?6 . "C-~")
    ( ?1 . "C-<")
    ( ?2 . "C->")
    ( ?3 . "C-?")
    )
  )

(defun kps (s)
  (let ((list
	 (mapcar '(lambda (x)
		   (or
		    (alist-get x function-key-to-kp-alist)
		    (format "C-%c" x)
		    ))
		 (string-to-list s))
	 ))
    (kbd (mconcat list " "))
    )
  )

(defun ks3 (s) (kps s))

;(key-description (kps "88"))
;(key-description (kps "^88"))

(define-key function-key-map (kbd "<C-kp-divide>") (kbd "C-/"))
(define-key function-key-map (kbd "<C-kp-multiply>") (kbd "C-*"))
(define-key function-key-map (kbd "<C-kp-subtract>") (kbd "C-_"))
(define-key function-key-map (kbd "<C-kp-add>") (kbd "C-+"))
(define-key function-key-map (kbd "<C-kp-decimal>") (kbd "C-^"))
(define-key function-key-map (kbd "<C-kp-7>") (kbd "C-{"))
(define-key function-key-map (kbd "<C-kp-8>") (kbd "C-}"))
(define-key function-key-map (kbd "<C-kp-9>") (kbd "C-!"))
(define-key function-key-map (kbd "<C-kp-4>") (kbd "C-:"))
(define-key function-key-map (kbd "<C-kp-5>") (kbd "C-$"))
(define-key function-key-map (kbd "<C-kp-6>") (kbd "C-~"))
(define-key function-key-map (kbd "<C-kp-1>") (kbd "C-<"))
(define-key function-key-map (kbd "<C-kp-2>") (kbd "C->"))
(define-key function-key-map (kbd "<C-kp-3>") (kbd "C-?"))
(define-key function-key-map (kbd "<C-kp-0>") (kbd "C-x C-h"))

(define-key function-key-map (kbd "<C-kp-enter>") (kbd "C-%"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <kp-divide>
;; <kp-multiply>
;; <kp-subtract>
;; <kp-add>
;; <kp-decimal>
;; <kp-0>
;; <kp-enter>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;+

;(describe-variable 'function-key-map)
;(describe-key (kbd "C--"))

;(dregf "kp-7" elfs "ever")

; (lookup-key function-key-map (kbd "C-:"))
; (lookup-key function-key-map (kbd "<kp-7>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define-key function-key-map (kbd "<kp-up>") (kbd "C-:"))
