
; (yas-new-snippet)
; (yas-expand-snippet "name: $1\n key: ${2:${1:$(yas--key-from-desc yas-text)}}")



name: op
key: op

(yas--key-from-desc "hello world")

yas-load-snippet-buffer-and-close

yas-snippet-dirs
yas-load-directory

C-c & C-n	yas-new-snippet
C-c & C-s	yas-insert-snippet
C-c & C-v	yas-visit-snippet-file

