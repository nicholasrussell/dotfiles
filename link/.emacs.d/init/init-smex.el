;;;; smex

(require 'smex)

(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
; Key Bindings for smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
; Old M-x
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


(provide 'init-smex)

