(display-buffer-use-some-window (get-buffer "*zooz*") nil)

(display-buffer-use-some-window (get-buffer "*zooz*")
 `((window-height . 20))
 )


(display-buffer-below-selected (get-buffer "*zooz*")
 `((window-height . 20))
 )


(display-buffer-at-bottom (get-buffer "*zooz*")
 `((window-height . 5))
 )


(display-buffer-pop-up-window (get-buffer "*zooz*")
 `((window-height . 10))
 )

(display-buffer-pop-up-frame (get-buffer "*zooz*")
 `((window-height . 10))
 )


(display-buffer-in-tab (get-buffer "*zooz*") nil)


(display-buffer-in-previous-window (get-buffer "*zooz*")
  `((previous-window . ,mw))
  )

(display-buffer (get-buffer "*zooz*") `((previous-window . ,mw)))

nil)

(defun grug (buf plist) (debug))

display-buffer-fallback-action
display-buffer-base-action
display-buffer-alist

(display-buffer (get-buffer-create "yo") `((mode . '(c-mode))))
(display-buffer (get-buffer-create "yo") `((grug display-buffer-in-previous-window) . ((mode . (c-mode)))))
(display-buffer (get-buffer-create "yo") `((display-buffer-at-bottom) . ((mode . (c-mode)))))
(display-buffer (get-buffer-create "yo") `((display-buffer-reuse-mode-window) . ((mode . (c-mode)))))
(display-buffer (get-buffer-create "yo") `((display-buffer-reuse-mode-window) . ((mode . c-mode))))
(display-buffer (get-buffer-create "yo") `((grug) . ((mode . c-mode))))

(display-buffer (get-buffer-create "yo") `((display-buffer-in-previous-window)))
(display-buffer (get-buffer-create "yo") `((display-buffer-in-previous-window) . ((reusable-frames . t))))

(display-buffer (get-buffer-create "yo"))
(display-buffer (get-buffer-create "yo"))

(display-buffer (get-buffer-create "*scratch*"))


((display-buffer--maybe-same-window
  display-buffer-reuse-window
  display-buffer--maybe-pop-up-frame-or-window
  display-buffer-in-previous-window
  display-buffer-use-some-window
  display-buffer-pop-up-frame))

(window-prev-buffers (current-window))
