;;; russell-leader.el -*- lexical-binding: t; -*-

(global-unset-key (kbd "C-SPC"))
(use-package general
  :config
  (general-define-key "<escape>" 'keyboard-escape-quit)
  (general-create-definer russell/global-leader
    :prefix "C-SPC")
  (general-create-definer russell/toggle-leader
    :prefix "C-SPC t"
    :which-key "toggles")
  (russell/toggle-leader
    "p" '(treemacs :which-key "treemacs")
    "t" '(consult-theme :which-key "choose theme")))

;; Key Hints
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 0.3))

(provide 'russell-leader)

