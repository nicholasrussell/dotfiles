;; Turn on Iteratively Do Things mode

(require 'ido)
(require 'ido-ubiquitous)

(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point nil)
(setq ido-auto-merge-work-directories-length -1)
(ido-ubiquitous-mode 1)


(provide 'init-ido)

