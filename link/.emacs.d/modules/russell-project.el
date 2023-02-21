;;; russell-project.el -*- lexical-binding: t; -*-

;; Prefer built-in project.el over projectile
;; (use-package projectile
;;   :diminish projectile-mode
;;   :config (projectile-mode)
;;   :init
;;   (let ((project-dirs '("~/dev" "~/sandbox"
;;                             	search-path '())))
;;     (dolist (project-dir project-dirs search-path)
;;       (when (file-directory-p project-dir
;;                            	(setq search-path (cons project-dir search-path)))))
;;     (setq projectile-project-search-path search-path))
;;   (setq projectile-switch-project-action #'projectile-dired))

;; (quelpa '(consult-projectile :fetcher git :url "https://gitlab.com/OlMon/consult-projectile.git"))
;; (russell/global-leader
;;    "p" '(projectile-command-map :which-key "consult-projectile"))

(use-package treemacs
  :init
  (setq treemacs-follow-after-init t
        treemacs-is-never-other-window t
        treemacs-sorting 'alphabetic-case-insensitive-asc))
;; (use-package treemacs-projectile
;;   :requires (treemacs projectile))
(use-package treemacs-magit
  :requires (treemacs magit))

(provide 'russell-project)

