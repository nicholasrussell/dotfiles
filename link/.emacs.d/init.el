;;; init.el -*- lexical-binding: t; -*-

;; Directory to place additional Emacs config
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(require 'russell-init)
(require 'russell-package)
(require 'russell-editor)
(require 'russell-leader)
(require 'russell-completion)
(require 'russell-term)
(require 'russell-vcs)
(require 'russell-project)
(require 'russell-ui)
(require 'russell-lang)

