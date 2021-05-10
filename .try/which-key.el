(setq which-key-popup-type 'frame)
(setq which-key-popup-type 'side-window)

(setq which-key-frame-max-height 40)

(which-key-show-full-keymap 'mbd-map)
(which-key-show-full-keymap 'qb-map)
(which-key-show-full-keymap 'minibuffer-local-completion-map)

(which-key-setup-side-window-right)
(which-key-setup-side-window-bottom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(top-level)
(call-interactively 'which-key-mode)

