; collection of all key-translation-map bindings we want to disable
; so that we can use the ALT key modifier effectively

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
(define-key function-key-map (kbd "<kp-1>") (kbd "C-<"))
(define-key function-key-map (kbd "<kp-2>") (kbd "C->"))
(define-key function-key-map (kbd "<kp-3>") (kbd "C-?"))
(define-key function-key-map (kbd "<kp-4>") (kbd "C-:"))
(define-key function-key-map (kbd "<kp-5>") (kbd "C-h"))
(define-key function-key-map (kbd "<kp-6>") (kbd "C-~"))
(define-key function-key-map (kbd "<kp-7>") (kbd "C-{"))
(define-key function-key-map (kbd "<kp-8>") (kbd "C-}"))
(define-key function-key-map (kbd "<kp-9>") (kbd "C-x C-h"))

;(describe-variable 'function-key-map)

