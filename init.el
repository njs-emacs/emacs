(defun byte-compile-log-warning (&rest args))

(load "nload" t t)
(load "plemu" t t)
(load "cl" t t)
(load "environ" t t)
(load "custom" t t)
(load "quick" t t)
(load "isearch" t t)
(load "fileclass" t t)
(load "n-autoload" t t)
(load "modes" t t)
(load "read" t t)
(load "show" t t)
(load "fileutil" t t)
(load "search" t t)
(load "unconcat" t t)
(load "nvc" t t)
(load "backup-grep" t t)
(load "bdiff" t t)
(load "dired" t t)
(load "pad" t t)

; placement of load "keys" is sensitive, as keys are defined in
; maps which only exist after loading of modules, e.g dired-mode-map
; this forces keys load to be done later
; however, some maps get defined in keys.el, which forces earlier load
; this conflict needs to be resolved.
; anything loaded before keys should not define-keys

(load "keys" t t)
(load "nlisp" t t)

(load "macros" t t)
(load "frequent" t t)
(load "qi" t t)
(load "link-buffer" t t)

(load "perl" t t)
(load "c-mode" t t)
;(load "cc-mode" t t)
(load "java" t t)

(load "defin" t t)

(load "confirm" t t)
(load "qhelp" t t)
(load "exec" t t)

(load "winutil" t t)
(load "mouse" t t)
(load "imenu-n" t t)

(load "frame" t t)

(load "path" t t)
(load "compile" t t)
(load "shell" t t)
(load "ediff" t t)
(load "findgrep" t t)
(load "ebg" t t)

(load "log3" t t)

(load "todo" t t)

(load-overrides "init")

;(load "d:/.data/e/emacs/init.el" t t t t)
;(load "c:/.data/e/emacs/init.el" t t t t)
(message "c: and d: init removed!")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ns-make-frame-function (frame)
  )

(setq after-make-frame-functions
  (adjoin 'ns-make-frame-function after-make-frame-functions))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq eval-expression-print-length nil)
(setq query-replace-highlight t)
(set-face-background (make-face 'region) "pink")

(show-paren-mode)
(setq kill-ring-max 1000)

(set-default 'truncate-lines t)
;(set-default 'line-move-visual t)

(make-variable-buffer-local 'auto-hscroll-mode)
(set-default 'auto-hscroll-mode t)

(setq history-length t)

(setq archive-zip-extract '("unzip" "-qq" "-c"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "filehist" t t)
(load "qi-html" t t)
(load "fly-spec" t t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "daily" t t)

(load "symlink" t t)
(load "gface" t t)
(load "vc-x" t t)
(load "n-all" t t)
(load "n-register" t t)
(load "yas" t t)
(load "recent" t t)

(define-key global-map "\C-xf" 'find-file)

(qb-define (control-key-vector ?e ?x) (concat ehome "/help/home.org"))
(qb-define (control-key-vector ?e ?t) (concat ehome "/help/tmp"))

(qb-define (control-key-vector ?d ?.) (daily-path ".emacs.el") t)
(qb-define (control-key-vector ?d ?e) (daily-path "0.el") t)

(qi-define (control-key-vector ?g ?#) "#\#nobackup##")

(setq h-slash-map (make-sparse-keymap))
(setq s-slash-map (make-sparse-keymap))

(define-key global-map (kbd "H-/") h-slash-map)
(define-key global-map (kbd "s-/") s-slash-map)

(setq create-lockfiles nil)
(setq list-command-history-max 4096)
