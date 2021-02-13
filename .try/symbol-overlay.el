NO define-transient-command

(define-transient-command symbol-overlay-transient ()
  "Symbol Overlay transient"
  ["Symbol Overlay"
   ["Overlays"
    ("." "Add/Remove at point" symbol-overlay-put)
    ("k" "Remove All" symbol-overlay-remove-all)
    ]
   ["Move to Symbol"
    ("n" "Next" symbol-overlay-switch-forward)
    ("p" "Previous" symbol-overlay-switch-backward)
    ]
   ["Other"
    ("m" "Hightlight symbol-at-point" symbol-overlay-mode)
    ]
   ]
  )
(global-set-key (kbd "s-.") 'symbol-overlay-transient)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defhydra symbol-overlay-hydra (:color pink :hint nil :timeout 30)
      "
  _p_   ^^   _b_  back         _h_  highlight  _i_  isearch
_<_   _>_    _d_  definition   _R_  remove     _Q_  query-replace
  _n_   ^^   _w_  save         ^^              _r_  rename
"
      ("<"      symbol-overlay-jump-first)
      (">"      symbol-overlay-jump-last)
      ("p"      symbol-overlay-jump-prev)
      ("n"      symbol-overlay-jump-next)

      ("d"      symbol-overlay-jump-to-definition)
      ("b"      symbol-overlay-echo-mark)

      ("h" symbol-overlay-put :color blue)
      ("R" symbol-overlay-remove-all :color blue)

      ("w" symbol-overlay-save-symbol :color blue)
      ("t" symbol-overlay-toggle-in-scope)

      ("i" symbol-overlay-isearch-literally :color blue)
      ("Q" symbol-overlay-query-replace :color blue)
      ("r" symbol-overlay-rename  :color blue)
      ("q" nil)
  )

(global-set-key (kbd "s-.") 'symbol-overlay-hydra/body)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
