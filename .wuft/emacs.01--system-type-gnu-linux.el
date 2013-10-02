(custom-set-faces
 ;;; ignoring dire warnings about this not working if in more than one place
 '(minibuffer-prompt
   ((((background dark)) (:foreground "yellow")))
   ))

;; this also works
(face-spec-set 'minibuffer-prompt `((t (:foreground "blue"))))

;; this doesn't
;; (face-spec-set 'minibuffer-prompt `((t (:foreground "red") (:weight bold))))

(set-scroll-bar-mode 'right)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for nvc.el

(setq nvc-remote-ntfs t)

(setq php-exe "php")

(setenv "PATH" (concat "~/bin:" (getenv "PATH")))
