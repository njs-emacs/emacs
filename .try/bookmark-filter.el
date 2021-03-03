(defun blop (bookmark)
  "Return non-nil if BOOKMARK is an autonamed bookmark.
BOOKMARK is a bookmark name or a bookmark record."
  (unless (stringp bookmark) (setq bookmark  (bmkp-bookmark-name-from-record bookmark)))
  (bmkp-string-match-p (format bmkp-autoname-format ".*") bookmark))

