(cond
 ((= emacs-major-version 24)
  (setq yas-load-path (// "boo/e/.p/yasnippet"))
  (let ((load-path (cons yas-load-path load-path)))
    (require 'yasnippet)
    )
  )
 ((> emacs-major-version 24)
  (require 'yasnippet)
  )
 )

(setq yas-my-dir (concat user-emacs-home "/yas"))

(setq yas-snippet-dirs
  (list
   "e:/home/nick/.emacs.d/snippets"
   yas-my-dir
;   yasnippet-snippets-dir
   )
  )

(add-mode "\\.ys$" 'snippet-mode)

(qb-define (kbd "C-e C-y") '(car yas-snippet-dirs))

(require 'yas-x)

;(load-file (// "boo/e/.p/tc/.yas/yas.el"))
;(load-file (// "boo/e/.p/stub/.yas/yas.el"))

(yas-global-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; yas-snippet-dirs
;;;;; ("e:/home/nick/.emacs.d/snippets" yasnippet-snippets-dir)
;;; yasnippet-snippets-dir = "e:/home/nick/.emacs.d/elpa/yasnippet-snippets-20190316.1019/snippets"
;;;   these are the snippets we don't want
;;; yas/root-directory is a variable defined in ‘yasnippet.el’.
;;;  This variable is an alias for ‘yas-snippet-dirs’.
;;;  The deprecated `yas/root-directory' aliases

;;; we need to clobber the yas-snippet-dirs before we
;;; call yas-global-mode

;;; yas-describe-tables

;;; we didn't need to compile/load snippet 'no' that we just created
;;; it became active right away...
;;; is this a new behaviour?
;;; "e:/ca/.emacs/.yas" isn't in yas-snippet-dirs
;;; what happens if we save a snippet in a random place (needs mode hint?)
;;; it gets loaded for mode named in parent directory, e.g faff

;;; as long as a file in snippet-mode is saved in a directory called
;;; perl-mode, it will be enabled in perl-mode
;;; that seems to be new behaviour
