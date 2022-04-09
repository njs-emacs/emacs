(require 'org)
(require 'org-capture)

; (setq org-directory "~/org")

(setq org-master-directory "e:/.org")

(setq org-master-file (filename-concat org-master-directory "home.org"))
(setq org-default-notes-file (filename-concat org-master-directory "notes.org"))
(setq org-help-file (filename-concat org-master-directory "help.org"))

(defun org-master-file-edit ()
  (interactive) (find-file-other-window org-master-file))

(defun org-default-notes-file-visit ()
  (interactive)
  (find-file org-default-notes-file)
  )

(defun org-help-file-visit ()
  (interactive)
  (find-file org-help-file)
  )

; global org-mode entry points

(def-key-global [C-f1] 'org-capture)
(def-key-global [C-f2] 'bookmark-bmenu-list)

(def-key-global [M-f1] 'org-cycle-agenda-files)
(def-key-global [M-f2] 'org-default-notes-file-visit)
(def-key-global [M-f3] 'org-agenda-switch-buffer)
(def-key-global [M-f4] 'org-agenda)
(def-key-global [M-f5] 'org-help-file-visit)

(defun org-agenda-switch-buffer (&optional arg)
  (interactive "p")
  (cond
   ((buffer-live-p org-agenda-buffer)
    (case arg
      (4 (switch-to-buffer-other-window org-agenda-buffer))
      (t (switch-to-buffer org-agenda-buffer))
      )
    )
   (t (org-agenda-list))
   )
  )

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

  (org-defkey org-mode-map (kbd "C-c l") 'org-store-link)
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
   `(
     (keymap-help . "e:/.org/.topic/org-mode/keymap.org")
     (mode-help . "e:/.org/.topic/org-mode/org-mode-help.org")
     )
   )

(file-class-linked-file-add 'org-agenda-mode
   `(
     (keymap-help . "e:/.org/.topic/org-mode/agenda.org")
     (mode-help . "e:/.org/.topic/org-mode/org-mode-help.org")
     )
   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-agenda-inactive ()
  (interactive)
  (let ((org-agenda-include-inactive-timestamps t))
    (org-agenda)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-refile-target-here ()
  (interactive)
  (setq org-refile-targets
    `((,(buffer-file-name) :tag . "refile_here"))
    )
  )
