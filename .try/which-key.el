;;; can't make this feature really useful yet
;;; it doesn't really add that much functionality over C-h

;; (setq which-key-popup-type 'frame)
;; (setq which-key-popup-type 'side-window)

; this seems to overcome the narrow side-windoe issue

(setq which-key-allow-imprecise-window-fit t)

(setq which-key-frame-max-height 80)

(which-key-setup-side-window-right)
;; (which-key-setup-side-window-left)
;; (which-key-setup-side-window-bottom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(top-level)

(which-key-show-full-keymap 'mbd-map)
(which-key-show-full-keymap 'qb-map)
(which-key-show-full-keymap 'minibuffer-local-completion-map)

(call-interactively 'which-key-mode)

(define-key global-map (kbd "C-! C-a") 'which-key-show-major-mode)

(define-key global-map (kbd "C-! C-s") 'which-key-toggle-docstrings)
(define-key global-map (kbd "C-! <next>") 'which-key-show-next-page-cycle)

(setq which-key-frame-max-height 80)
(setq which-key-frame-max-width 120)
(setq which-key-frame-max-width 90)
(setq which-key-frame-max-width 60)

(setq which-key-side-window-max-height 0.99)
(setq which-key-side-window-max-width 0.8)
(setq which-key-side-window-max-width 620)
(setq which-key-side-window-max-width 90)

(which-key--width-or-percentage-to-width 100)

(which-key--side-window-max-dimensions)
(which-key--frame-max-dimensions)

;; default
(setq which-key-sort-order 'which-key-key-order)

;; same as default, except single characters are sorted alphabetically
;; (setq which-key-sort-order 'which-key-key-order-alpha)

;; same as default, except all prefix keys are grouped together at the end
;; (setq which-key-sort-order 'which-key-prefix-then-key-order)

;; same as default, except all keys from local maps shown first
;; (setq which-key-sort-order 'which-key-local-then-key-order)

;; sort based on the key description ignoring case
;; (setq which-key-sort-order 'which-key-description-order)
