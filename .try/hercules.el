;; this looks cool, but there are issues with removing the advice
;; which gets applied to the show-funs and hide-funs
;;

(hercules-def
 :show-funs #'windresize
 :hide-funs '(windresize-exit windresize-cancel-and-quit)
 :whitelist-keys '("o" "p" "q" ,(kbd "<up>"))
 :keymap 'windresize-map)
 
(define-key global-map (kps "965") #'windresize)

windresize
