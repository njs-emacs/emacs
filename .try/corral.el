(defhydra hydra-corral (:columns 4)
    "Corral"
    ("(" corral-parentheses-backward "Back")
    (")" corral-parentheses-forward "Forward")
    ("[" corral-brackets-backward "Back")
    ("]" corral-brackets-forward "Forward")
    ("{" corral-braces-backward "Back")
    ("}" corral-braces-forward "Forward")
    ("}" corral-braces-forward "Forward")
    ("n" forward-sexp "fx")
    ("p" backward-sexp "bx")
    ("<left>" backward-char "fc")
    ("<right>" forward-char "bc")
    ("." hydra-repeat "Repeat"))
(global-set-key (kbd "C-v C-c") #'hydra-corral/body)

;
(demo)
{[(demo)] with} {more} {than} [one word]
[demo with more than one] word
[demo with more than] [one] [word]
[demo with] [more] [than] [one word]
demo with more than one word
[demo with more than one word]
[demo with more than one] word
[demo with] [more] [than] [one] [word]
[demo with more than one word]

[demo with] more than [one word]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; corral.el

;;;(buffer-swap-text (get-buffer ".try/corral.el"))
;;;(buffer-swap-text (get-buffer "/keys.el"))
