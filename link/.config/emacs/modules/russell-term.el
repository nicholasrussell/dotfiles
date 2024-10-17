;;; russell-term.el -*- lexical-binding: t; -*-

; Fix shell PATH
(exec-path-from-shell-initialize)

(defun russell/set-no-process-query-on-exit ()
  (let ((proc (get-buffer-process (current-buffer))))
    (when (processp proc)
      (set-process-query-on-exit-flag proc nil))))

(setq vterm-always-compile-module t)

(require 'vterm)

(setq vterm-max-scrollback 10000)
(setq vterm-kill-buffer-on-exit t)
(add-hook 'vterm-mode-hook 'russell/set-no-process-query-on-exit)
(keymap-global-set "C-`" 'vterm)

(provide 'russell-term)

