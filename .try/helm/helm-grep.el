;; mods to helm-grep to insert default-directory: local variable
;; and provide set-default-directory (kps "57") 

(defmacro with-helm-buffer-if-live (&rest body)
  "Eval BODY inside `helm-buffer'."
  (declare (indent 0) (debug t))
  (let ((buffer (get-buffer (helm-buffer-get))))
    (cond
     ((buffer-live-p buffer)
      `(with-current-buffer ,buffer ,@body))
     )
    )
  )

;; (with-helm-buffer-if-live (+ 1 2))
;; (macroexpand '(with-helm-buffer-if-live (+ 1 2)))

;; overrides built-in helm-grep

(define-derived-mode helm-grep-mode
    special-mode "helm-grep"
    "Major mode to provide actions in helm grep saved buffer.

Special commands:
\\{helm-grep-mode-map}"
    (set (make-local-variable 'helm-grep-last-cmd-line)
         (with-helm-buffer-if-live helm-grep-last-cmd-line))
    (set (make-local-variable 'revert-buffer-function)
         #'helm-grep-mode--revert-buffer-function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun set-default-directory (dir)
  (interactive
    (let ((dir (sx (bob) (find-match-string "default-directory: \"\\(.*\\)\"" 1))))
      (list (read-from-minibuffer "Where: " dir))
      )
    )
;  (debug)
;  (setq default-directory (read-from-minibuffer "Where: " nil nil nil 'ploxa-history))
  (setq default-directory dir)
  (message "default-directory is %s" default-directory)
  )

(def-key global-map (kps "57") 'set-default-directory)

; (defvar ploxa-history nil)
;(add-hook 'helm-grep-before-init-hook 'ploxa)
;(add-hook 'helm-grep-after-init-hook 'ploxa)
;(add-hook 'helm-grep-mode-hook 'ploxa)

; (dregf "helm-grep" elfs "year")
; (dregf "default-directory" elfs "year")
