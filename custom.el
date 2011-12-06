(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq enable-local-eval t)

(setq inhibit-startup-message t)

(setq search-exit-char 0)
(setq search-delete-char 8)

(setq blink-matching-paren-distance 200000)
(setq scroll-step 1)

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq require-final-newline t)

(setq mail-self-blind t)

(setq window-min-height 2)

(setq-default case-fold-search nil)
(setq-default next-screen-context-lines 0)

(modify-syntax-entry ?_ "w")

(setq suggest-key-bindings nil)

(load-overrides "custom")
