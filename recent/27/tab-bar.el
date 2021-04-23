(defun tab-bar-tab-name-current-fixed ()
  "Generate tab name from the buffer of the selected window."
  (format "%-24s" (buffer-name (window-buffer (minibuffer-selected-window)))))

(setq tab-bar-tab-name-function 'tab-bar-tab-name-current-with-count)
(setq tab-bar-tab-name-function 'tab-bar-tab-name-current-fixed)

(def-key-global (kbd "<H-left>") 'tab-previous)
(def-key-global (kbd "<H-right>") 'tab-next)

(def-key-global (kps "+4") 'tab-previous)
(def-key-global (kps "+6") 'tab-next)

(custom-set-faces
'(tab-bar-tab ((t (:inherit tab-bar
;			    :background "honeydew"
			    :background "cornsilk"
			    :box (:line-width 1 :style released-button)))))
 )
