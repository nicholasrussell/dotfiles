;; Better buffer names

(require 'uniquify)

(setq uniquify-buffer-name-style 'forward) ; 'post-forward
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

(provide 'init-uniquify)

