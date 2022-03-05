(require 'org)

; (setq org-directory "~/org")

(setq org-master-directory "e:/.org")

(setq org-master-file (filename-concat org-master-directory "home.org"))

(setq org-default-notes-file (filename-concat org-master-directory "notes.org"))

(defun org-master-file-edit ()
  (interactive) (find-file-other-window org-master-file))

(defun org-default-notes-file-visit ()
  (interactive)
  (find-file org-default-notes-file)
  )

(def-key-global [C-f1] 'org-capture)
(def-key-global [C-f2] 'bookmark-bmenu-list)

(def-key-global [M-f1] 'org-cycle-agenda-files)
(def-key-global [M-f2] 'org-default-notes-file-visit)
(def-key-global [M-f3] 'org-agenda)

(def-key global-map (kbd "<M-S-f1>") 'org-master-file-edit)

(def-key-global (kbd "C-c a") 'org-agenda)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ns-org-load-hook ()
  (add-hook 'org-mode-hook 'turn-on-font-lock)
  (setq org-file-apps (append org-file-apps (list (cons t 'emacs))))
  (setq org-open-directory-means-index-dot-org nil)
  (org-defkey org-mode-map (kbd "C-'") nil)
  (org-defkey org-mode-map (kbd "C-,") nil)

  (org-defkey org-mode-map (kbd "C-c C-;") c-colon-map)
  )

(ns-org-load-hook)

;; org-directory is a variable defined in ‘org.el’.
;; Its value is "~/org"
;;
;; this is analogous to org-master-directory which is
;; Its value is "e:/.org"
;;
;; the org-mode package will normally use org-directory
;; and this may change during a session depending upon
;; where we are working
;;
;; org-master is a permanent org file repository which
;; we always want to be in the same place
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-capture-template-add (key template)
  (setq org-capture-templates (alist-put org-capture-templates key template))
  )

(defun org-capture-template-remove (key)
  (setq org-capture-templates (alist-remprop org-capture-templates key))
  )

(file-class-linked-file-add 'org-mode
   `((keymap-help . "e:/.org/.topic/org-mode/keymap.org")))

