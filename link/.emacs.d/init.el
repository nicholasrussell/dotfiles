;;;; Emacs init.el
;; Configures Emacs

;; Load init files
(add-to-list 'load-path (expand-file-name "init" user-emacs-directory))

(require 'init-utils)     ; Utils
(require 'init-elpa)      ; Package repositories and package.el

;; Package archives
;; Install packages
(defvar emacs-packages '(ac-cider
                         auto-complete
                         cider
                         cljsbuild-mode
                         clojure-mode
                         color-theme-sanityinc-tomorrow
                         ; csv-mode
                         ; csv-nav
                         dash
                         ; diminish
                         doremi
                         doremi-cmd
                         ; eldoc
                         ; elein
                         ; elpy ; Emacs Lisp Python Environment
                         epl
                         exec-path-from-shell
                         f
                         github-theme
                         ; go-mode
                         ; groovy-electric
                         groovy-mode
                         haskell-mode
                         ht
                         icicles
                         ido
                         ; inf-groovy
                         ; jedi ; Python auto-completion
                         js2-mode
                         julia-mode
                         less-css-mode
                         load-theme-buffer-local
                         magit
                         markdown-mode
                         ; mmm-mode
                         monokai-theme
                         ; multiple-cursors
                         ; mwe-log-commands
                         neotree
                         nyan-mode
                         org
                         paredit
                         ; persistent-scratch
                         pkg-info
                         ; popwin
                         ; project-local-variables
                         queue
                         ; racket-mode
                         rainbow-delimiters
                         restclient
                         s
                         saveplace
                         ; scala-mode2
                         scratch
                         ; skewer-mode
                         slime
                         smart-mode-line
                         smartparens
                         smex
                         solarized-theme
                         ; sublimity
                         ; tabbar
                         tangotango-theme
                         web-mode
                         wgrep
                         zenburn-theme

                         dunnet)) ; because yes
(dolist (p emacs-packages)
  (unless (package-installed-p p)
    (package-install p)))

;; Set scratch message
(setq initial-scratch-message nil)

;; Load conifgs for specific features and modes
(require 'wgrep)
(require 'scratch)
;(require 'persistent-scratch)
(require 'mwe-log-commands)

(require 'init-themes)
(require 'init-locales)
(require 'init-editing)
(require 'init-parens)
(require 'init-org)
(require 'init-ido)
(require 'init-smex)
(require 'init-uniquify)
(require 'init-neotree)
(require 'init-slime)
(require 'init-backup)
(require 'init-search)
(require 'init-icicles)
(require 'init-save-places)
(require 'init-smart-mode-line)
(require 'init-common-lisp)
(require 'init-scheme)
(require 'init-clojure)
(require 'init-haskell)
(require 'init-web-mode)
(require 'init-js2-mode)
(require 'init-markdown)
(require 'init-sensitive-mode)

;; Allow access from emacsclient
(require 'server)
(unless (server-running-p)
  (server-start))

;; Variables configured via the interactive 'customize' interface
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Load optional init-local file containing personal settings
(require 'init-local nil t)

;; Open TODO file
(if (file-exists-p "~/org/TODO.org")
    (progn
      (find-file "~/org/TODO.org")
      (switch-to-buffer "*scratch*")))

;; Log load time
(add-hook 'after-init-hook
          (lambda ()
            (message "init completed in %.2fms"
                     (* 1000 (float-time (time-subtract after-init-time before-init-time))))))


(provide 'init)


