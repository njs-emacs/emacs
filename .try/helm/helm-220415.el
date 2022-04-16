(set-face-background (make-face 'helm-M-x-key) "white")
(set-face-foreground (make-face 'helm-M-x-key) "red")
(set-face-foreground (make-face 'helm-M-x-key) "darkred")
(set-face-foreground 'helm-M-x-key "red")

(helm :sources (helm-build-sync-source "test"
                 :candidates '(foo foa fob bar baz)
                 :fuzzy-match t)
      :buffer "*helm test*")

helm-mode-fuzzy-match

helm-show-kill-ring
helm-buffers-list
helm-find-files

(def-key global-map (kbd "H-x") 's-call-interactively)
(def-key global-map (kps "15") 'helm-find-files)

helm-prefix ; doesn't exist

(substitute-command-keys "\\{helm-find-files-map}")
