(setq nl-spy-exe (format "%sspy/__w/spy.exe" devhome))
(set-default 'nl-exec-program (format "%s -D9 -mpl" nl-spy-exe))
(set-default 'nl-buffer-eval-fun 'nl-buffer-eval-compile)
