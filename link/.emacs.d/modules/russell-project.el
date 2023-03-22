;;; russell-project.el -*- lexical-binding: t; -*-

(use-package treemacs
  :init
  (setq treemacs-follow-after-init t
        treemacs-is-never-other-window t
        treemacs-sorting 'alphabetic-case-insensitive-asc))

(use-package treemacs-magit
  :requires (treemacs magit))

(provide 'russell-project)

