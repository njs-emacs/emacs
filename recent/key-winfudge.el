;; windows super key fudge needs the autohotkey script e:/ahk/a.ahk running

(defvar wsk-alist nil "windows super key fudge")

(setq wsk-alist
  `(
    (?c . ?~)
    (?v . ?@)
    (?w . ?:)
    (?p . ?\;)
    (?l . ?&)
    ))

(defun wsk (key &optional keys)
  (cond ((stringp key) (setq key (string-to-char key))))
   (format "s-%c %s"
    (or (alist-get key wsk-alist) key)
    (or keys "")
   )
  )

(defun kwsk (key &optional keys)
  (kbd (wsk key keys))
  )

;; (setq sc-keymap (make-sparse-keymap))
;; (define-key global-map (kwsk "c") sc-keymap)

;; 
;;     
;; (wsk "a" "b")
;; (wsk "v" "b")
;; (kwsk "c" "b")
;;
;; (define-key sc-keymap (kbd "a") "test")

