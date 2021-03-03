(require 'hydra)

(defvar safe-key-symbol-alist nil)

(setq safe-key-symbol-alist
  '(
    ("{" . "lbrace")
    ("}" . "rbrace")

    ("[" . "lbracket")
    ("]" . "rbracket")
    
    (":" . "colon")
    (";" . "semicolon")
    ("@" . "at")
    ("#" . "hash")
    ("~" . "tilde")
    
    ("<" . "lt")
    (">" . "gt")
    
    ("," . "comma")
    ("." . "dot")
    
    ("?" . "question")
    ("!" . "bang")
    ("/" . "slash")
    ("\\" . "bslash")
    ("'" . "squote")
    ("\"" . "dquote")
    ))

(defun key-to-safe-symbol (k)
  (let* ((kd (gget safe-key-symbol-alist k)))
    (cond
     (kd)
     (k)
     )
    )
  )

;(key-to-safe-symbol ":")
;(c-key-to-safe-file-name "C-:")
;(key-to-safe-symbol ">")
;(c-key-to-safe-file-name "C->")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun key-to-file-name (name &optional prefix)
  (format "e:/emacs/keys/%s%s.el" (or prefix "") name)
  )

(defun key-to-safe-file-name (k &optional prefix)
  (key-to-file-name (key-to-safe-symbol k) prefix)
  )

(defun c-key-to-safe-file-name (k)
  (key-to-safe-file-name (substring k 2) "c-")
  )

(defun keycode-to-safe-symbol (key)
  (let* ((list (mapcar '(lambda (x) (cons (aref (kbd (concat "C-" (car x))) 0) x))
		      safe-key-symbol-alist))
	 (item (alist-get key list))
	 )
    (cond (item (cdr item))
	  ((char-to-string (+ key #x60)))
	  )
    ))

;(keycode-to-safe-symbol (aref (kbd "C-?") 0))
;(keycode-to-safe-symbol (aref (kbd "C-e") 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun keydef-edit-init ()
  (interactive)
  (let* ((keys (this-command-keys))
	 (key (aref keys 0))
	 (name (keycode-to-safe-symbol key))
	 )
    (cond
     (name (find-file-other-window (key-to-file-name name "c-")))
     )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-help-buffer-key-prefix ()
  (sx (set-buffer "*Help*")
      (bob)
      (rsf "Starting With \\(.*\\):$")
      (let ((s (ms 1)))
	s)
      )
  )

(defun binding-edit ()
  (let* ((key (get-help-buffer-key-prefix)))
    (find-file-other-window (c-key-to-safe-file-name key))
    )
  )

(eval
 `(defhydra binding-help-hydra (:foreign-keys warn :hint nil)
      "

  `   \\   |            :   @   ~  ]  {  }
  _q_   _e_                ;   '   #
      j                <   >   ?
  z   v                ,   .   /
"
  ("C-g"	nil)
  ("q"		nil :color blue)
  ("e"		(binding-edit) :color blue)

  ,@(apply 'append
 (mapcar '(lambda (key)
	     (let* ((kcs (format "C-%s" key))
		    (kck (kbd kcs))
		    (kms (format "M-%s" key))
		    )
	       (list
		 (list key `(describe-bindings ,kck))
		 (list kcs `(describe-bindings ,kck))
		 )))
	  `(
	    "v"
	    "j"
	    "`"
	    "\\"
	    "|"
	    ","
	    "."
	    "/"
	    "<"
	    ">"
	    "?"
	    ";"
	    "'"
	    "#"
	    ":"
	    "@"
	    "~"
	    "]"
	    "{"
	    "}"
	    )
	  ))  
  ))

(define-key global-map (kbd "C-; C-b") 'binding-help-hydra/body)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defhydra modifier-binding-help-hydra (:color blue :pre nil :post nil)
  "Bookmark menu hydra
"
  ("A" (find-file-other-window (key-to-safe-file-name "alt")) "Alt" :column "Single:")
  ("H" (find-file-other-window (key-to-safe-file-name "hyper")) "Hyper")
  ("S" (find-file-other-window (key-to-safe-file-name "super")) "Super")

  ("ac" (find-file-other-window (key-to-safe-file-name "alt-ctrl")) "A-C" :column "Combo:")
  ("ah" (find-file-other-window (key-to-safe-file-name "alt-hyper")) "A-H")
  ("am" (find-file-other-window (key-to-safe-file-name "alt-meta")) "A-M")
  ("as" (find-file-other-window (key-to-safe-file-name "alt-super")) "A-s")

  ("ch" (find-file-other-window (key-to-safe-file-name "ctrl-hyper")) "C-H")
  ("cm" (find-file-other-window (key-to-safe-file-name "ctrl-meta")) "C-M")
  ("cs" (find-file-other-window (key-to-safe-file-name "ctrl-super")) "C-s")

  ("hm" (find-file-other-window (key-to-safe-file-name "hyper-meta")) "H-M")
  ("hs" (find-file-other-window (key-to-safe-file-name "hyper-super")) "H-s")

  ("q" nil "Quit" :color blue :column "Other:")

  )

(define-key global-map (kbd "C-; C-v") 'modifier-binding-help-hydra/body)

