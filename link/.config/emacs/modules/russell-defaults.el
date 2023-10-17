;;; russell-defaults.el -*- lexical-binding: t; -*-

;;; Buffers
;; Revert Dired and other buffers
(customize-set-variable 'global-auto-revert-non-file-buffers t)
;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)
;; Make dired do something intelligent when two directories are shown
;; in separate dired buffers.  Makes copying or moving files between
;; directories easier.  The value `t' means to guess the default
;; target directory.
(customize-set-variable 'dired-dwim-target t)
;; automatically update dired buffers on revisiting their directory
(customize-set-variable 'dired-auto-revert-buffer t)
;; pop up dedicated buffers in a different window.
(customize-set-variable 'switch-to-buffer-in-dedicated-window 'pop)
;; treat manual buffer switching (C-x b for example) the same as
;; programmatic buffer switching.
(customize-set-variable 'switch-to-buffer-obey-display-actions t)
;; The forward naming method includes part of the file’s directory name at the beginning of the buffer name; using this method, buffers visiting the files /u/rms/tmp/Makefile and /usr/projects/zaphod/Makefile would be named ‘tmp/Makefile’ and ‘zaphod/Makefile’.
(setq uniquify-buffer-name-style 'forward)
;; Set default dired listing behavior
(setq dired-listing-switches "-alghov --group-directories-first --time-style=long-iso")
(when russell/env-mac-os-p
  (setq insert-directory-program "gls"
        dired-use-ls-dired t))

;;; Completion
;; No matter which completion mode is used:
(customize-set-variable 'tab-always-indent 'complete)
(customize-set-variable 'completion-cycle-threshold 3)
(customize-set-variable 'completion-category-overrides
                        '((file (styles . (partial-completion)))))
(customize-set-variable 'completions-detailed t)
;; use completion system instead of popup window
(customize-set-variable 'xref-show-definitions-function
                        #'xref-show-definitions-completing-read)

;;; Sessions
;; Turn on recentf mode
(add-hook 'after-init-hook #'recentf-mode)
;; Enable savehist-mode for command history
(savehist-mode 1)
;; save the bookmarks file every time a bookmark is made or deleted
;; rather than waiting for Emacs to be killed.  Useful especially when
;; Emacs is a long running process.
(customize-set-variable 'bookmark-save-flag 1)

;;; Window Management
;; Make scrolling less stuttered
(setq auto-window-vscroll nil)
(customize-set-variable 'fast-but-imprecise-scrolling t)
(customize-set-variable 'scroll-conservatively 101)
(customize-set-variable 'scroll-margin 0)
(customize-set-variable 'scroll-preserve-screen-position t)
;; Window configuration for special windows.
(add-to-list 'display-buffer-alist
             '("\\*Help\\*"
               (display-buffer-reuse-window display-buffer-pop-up-window)
               (inhibit-same-window . t)))
(add-to-list 'display-buffer-alist
             '("\\*Completions\\*"
               (display-buffer-reuse-window display-buffer-pop-up-window)
               (inhibit-same-window . t)
               (window-height . 10)))

;;; Backups
(defvar russell/saves-dir (file-name-concat russell/emacs-data-home "saves/"))
(when (not (file-exists-p russell/saves-dir))
  (make-directory russell/saves-dir t))
(setq backup-by-copying t)
(setq backup-directory-alist `(("." . ,russell/saves-dir)))
(setq auto-save-file-name-transforms `((".*" ,russell/saves-dir t)))
(setq delete-old-versions t)
(setq kept-old-versions 1)
(setq kept-new-versions 1)
(setq version-control t)

;;; Editing
;; Typed text replaces the selection if the selection is active,
;; pressing delete or backspace deletes the selection.
(delete-selection-mode)
;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)
;; Do not save duplicates in kill-ring
(customize-set-variable 'kill-do-not-save-duplicates t)
;; Better support for files with long lines
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(global-so-long-mode 1)
;; define a key to define the word at point.
(keymap-set global-map "M-#" #'dictionary-lookup-definition)
;; turn on spell checking, if available.
(with-eval-after-load 'ispell
  (when (executable-find ispell-program-name)
    (add-hook 'text-mode-hook #'flyspell-mode)
    (add-hook 'prog-mode-hook #'flyspell-prog-mode)))

;;; Projects
(require 'project)

;;; Undo system
;; Use undo-tree
(require 'undo-tree)
(customize-set-variable 'undo-tree-history-directory-alist `(("." . ,(file-name-concat russell/emacs-cache-home "undo"))))
(setq undo-tree-visualizer-diff t
      undo-tree-auto-save-history t
      undo-tree-enable-undo-in-region t
      ;; Increase undo limits to avoid emacs prematurely truncating the undo
      ;; history and corrupting the tree. This is larger than the undo-fu
      ;; defaults because undo-tree trees consume exponentially more space,
      ;; and then some when `undo-tree-enable-undo-in-region' is involved. See
      ;; syl20bnr/spacemacs#12110
      undo-limit 800000           ; 800kb (default is 160kb)
      undo-strong-limit 12000000  ; 12mb  (default is 240kb)
      undo-outer-limit 128000000) ; 128mb (default is 24mb)
(global-undo-tree-mode 1)

(provide 'russell-defaults)

