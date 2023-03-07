;;; russell-term.el -*- lexical-binding: t; -*-

(defun russell/set-no-process-query-on-exit ()
  (let ((proc (get-buffer-process (current-buffer))))
    (when (processp proc)
      (set-process-query-on-exit-flag proc nil))))

(use-package vterm
  :config
  (setq vterm-max-scrollback 10000)
  (setq vterm-kill-buffer-on-exit t))
(add-hook 'vterm-mode-hook 'russell/set-no-process-query-on-exit)

(provide 'russell-term)

