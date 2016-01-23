;; change [f9] to the key you prefer to activate the ruler with.
 (global-set-key [s-f9] 'my-column-ruler)
 
  (defun my-column-ruler (width)
   "Display temp ruler at point."
   (interactive `(,(+ (window-hscroll)(window-width))))
   (momentary-string-display
    (let* ((iterations (/ (1- width) 10))
           (result1 "|...|....|")
           (result2 "1   5   10")
           (inc1 "....|....|")
           (inc2 "        %d0")
           (i 1))
      (while  (<= i iterations)
        (setq i (1+ i))
        (setq result1 (concat result1 inc1))
        (setq result2 (concat result2 (substring (format inc2 i) -10))))
      (concat (substring result2 0 width) "\n"
              (substring result1 0 width) "\n"))
    (line-beginning-position)
    nil "[space] Clears ruler"))

(global-set-key [s-f9] 'my-column-ruler)
(global-set-key (kbd "C-' C--") 'popup-ruler)
(global-set-key (kbd "C-' C-|") 'popup-ruler-vertical)
