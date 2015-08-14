;;;; Common Lisp

(setq inferior-lisp-program "/opt/sbcl/bin/sbcl")

(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)


(provide 'init-common-lisp)

