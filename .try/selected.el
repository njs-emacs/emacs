(require 'selected)
(define-key selected-keymap (kbd "q") 'selected-off)
(define-key selected-keymap (kbd "u") 'upcase-region)
(define-key selected-keymap (kbd "d") 'downcase-region)
(define-key selected-keymap (kbd "w") 'count-words-region)
(define-key selected-keymap (kbd "m") 'apply-macro-to-region-lines)

;; mc/cycle-backward
;; mc/

u :: upper case
l :: lower case
k :: capitalise region
x :: cut
c :: copy
C :: duplicate region
y :: replace with clipboard contents
t :: reverse by char
T :: reverse by word
q :: string insert rectangle
Q :: insert range of numbers
r :: cut rectangle
g :: deselect region
m :: highlight region
M :: highlight region and all identical sections
M-up / M-down :: move selection up/down
Del :: delete region
s :: spellcheck region
f :: fill region with char
= :: evaluate with calculator
! :: pass region as input to external program
; :: comment/uncomment region
b :: open external browser with web search for selection
h :: show key map
n :: search for next text matching selection
p :: search for previous text matching selection
Space :: swap point and mark
Ret :: switch to rectangular selection mode

z :: undo in region
> :: indent
< :: unindent
w :: count words in region
a :: sort lines alphabetically
