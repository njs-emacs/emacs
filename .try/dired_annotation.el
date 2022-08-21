(use-package dired-annotator
  :load-path "e:/emacs/packages/emacs-dired-annotator"
  :bind (:map dired-mode-map
              ("C-c a s" . #'dired-annotator-show-icons)
              ("C-c a h" . #'dired-annotator-hide-icons)
              ("C-c a k" . #'dired-annotator-delete-note)
              ("C-c a o" . #'dired-annotator-edit-note)
              ("C-c a e" . #'dired-annotator-edit-note)
              ("C-c a a" . #'dired-annotator-show-note)
              ("'" . #'dired-annotator-show-note)))
