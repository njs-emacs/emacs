(defun byte-compile-log-warning (&rest args))
(load "plemu" t t)
(load "environ" t t)
(load "custom" t t)
(load "quick" t t)
(load "isearch" t t)
(load "fileclass" t t)
(load "autoload" t t)
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

(load "d:/.data/e/emacs/init.el" t t t t)
(load "c:/.data/e/emacs/init.el" t t t t)

(set-default 'truncate-lines t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ns-make-frame-function (frame)
  )

(setq after-make-frame-functions
  (adjoin 'ns-make-frame-function after-make-frame-functions))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq query-replace-highlight t)
(set-face-background (make-face 'region) "pink")

(show-paren-mode)
(setq kill-ring-max 1000)

;(require 'ffap)
;(ffap-bindings)

(setq archive-zip-extract '("unzip" "-qq" "-c"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "filehist" t t)
(load "qi-html" t t)
(load "fly-spec" t t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "daily" t t)

(load "symlink" t t)
(load "gface" t t)
(load "recent" t t)
