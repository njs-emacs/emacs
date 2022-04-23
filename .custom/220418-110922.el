;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "e:/ca/htdocs/pop/pop.bmk")
 '(compilation-ask-about-save nil)
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(gdb-non-stop-setting nil)
 '(ls-lisp-verbosity nil)
 '(magit-commit-arguments nil)
 '(org-agenda-files
   '("e:/.org/notes.org" "e:/.p/stub/index.org" "e:/.p/stub/new.org"))
 '(org-use-speed-commands t)
 '(package-selected-packages
   '(windresize hercules org-alert emacsql-sqlite emacsql emacsql-sqlite3 emacsql-psql emacsql-mysql persp-mode 0x0 0xc marginalia embark aggressive-indent chronos use-package org-roam svg-mode-line-themes svg-clock uuid button-lock powerline selected which-key names helm-org-rifle restclient-helm dynamic-ruler org-wild-notifier svg helpful w32-browser symbol-overlay avy org-journal ov deft corral org yasnippet yasnippet-snippets goto-last-change helm helm-ebdb move-text dired-subtree dired+ dired-filter all-the-icons-dired undohist undo-tree ranger wgrep w3 vlf unbound swiper slime-js skewer-mode restclient projectile phi-search paredit multiple-cursors magit impatient-mode hydra expand-region engine-mode emms elnode ecb dirtree command-log-mode cider bookmark+ auto-yasnippet all ace-window ace-jump-buffer))
 '(safe-local-variable-values
   '((dummy emacs-read-hash-plus)
     (first-change-hook not-this-file)
     (outline-minor-mode)
     (whitespace-style face tabs spaces trailing lines space-before-tab::space newline indentation::space empty space-after-tab::space space-mark tab-mark newline-mark)
     (log-short-time-insert-format . "%d_%H%M")
     (file-class . daily-emacs)
     (panic-immune . t)
     (eval-buffer-function . perl-process)
     (cperl-continued-statement-offset . 2)
     (cperl-continued-brace-offset . 0)
     (cperl-brace-offset . 0)
     (cperl-brace-imaginary-offset . 0)
     (cperl-label-offset . -2)
     (first-change-hook not-this-file)
     (yub . n)
     (humb)
     (yub . y)
     )
   )
 '(tooltip-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :background "LightGray" :foreground "black" :height 80 :foundry "outline" :family "Lucida Console"))))
 '(fixed-pitch ((t nil)))
 '(font-lock-string-face ((((class color) (min-colors 88) (background light)) (:foreground "darkviolet"))))
 '(font-lock-variable-name-face ((t (:foreground "sienna4"))))
 '(fringe ((t (:background "#E0E0C0" :foreground "#A0A0A0"))))
 '(highlight-changes-face ((((class color)) (:background "yellow" :foreground "red"))) t)
 '(mode-line-buffer-id ((t (:weight bold :height 1.1))))
 '(org-hide ((((background light)) (:foreground "LightGray"))))
 '(tab-bar-tab ((t (:inherit tab-bar :background "honeydew" :box (:line-width 1 :style released-button))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this has been hacked so that Custom made custom-set-faces cannot override
;; what we really want

(setq default-custom-set-face-list
  (list 
;   '(default ((t (:inherit nil :background "LightGray" :foreground "black" :height 80 :foundry "outline" :family "Lucida Console"))))
   '(default ((t (:inherit nil :height 80 :foundry "outline" :family "Lucida Console"))))
   '(font-lock-string-face ((((class color) (min-colors 88) (background light)) (:foreground "darkviolet"))))
   '(fringe ((t (:background "#E0E0C0" :foreground "#A0A0A0"))))
   '(highlight-changes-face ((((class color)) (:background "yellow" :foreground "red"))) t)
   '(mode-line-buffer-id ((t (:weight bold :height 1.1))))
   '(org-hide ((((background light)) (:foreground "LightGray")))))
  )

(apply 'custom-set-faces default-custom-set-face-list)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
