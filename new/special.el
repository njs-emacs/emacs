(setq special-display-regexps nil)

(defun adjoin-special (x)
  (setq special-display-regexps
    (adjoin x special-display-regexps)))

(adjoin-special "*Compilation.*")
