;;; russell-package.el -*- lexical-binding: t; -*-

(require 'package)
(require 'time-date)

(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))

(customize-set-variable 'package-archive-priorities
                        '(("gnu"    . 99)   
                          ("nongnu" . 90)
                          ("melpa-stable" . 80)
                          ("melpa"  . 0)
                          ("org" . 0)))

(use-package quelpa
  :config
  (setq quelpa-upgrade-interval 7)
  (setq quelpa-update-melpa-p nil)
  (setq quelpa-checkout-melpa-p nil)
  :hook
  (after-init-hook . quelpa-upgrade-all-maybe))

(package-initialize)

(if package-archive-contents
  ;; Check if packages are stale (older than n days)
  (when (cl-some
         (lambda (archive)
           (let* ((today (decode-time nil nil t))
                  (archive-name (expand-file-name
                                 (format "archives/%s/archive-contents" archive)
                                 package-user-dir))
                  (last-update-time (decode-time (file-attribute-modification-time
                                                  (file-attributes archive-name))))
                  (delta (make-decoded-time :day 1)))
             (time-less-p (encode-time (decoded-time-add last-update-time delta))
                          (encode-time today))))
         (mapcar #'car package-archives))
    (progn
      (message "russell/package: Package archives stale, refreshing")
      (package-refresh-contents t))) ;; in the background
  (progn
    (message "russell/package: Package archives empty, refreshing")
    (package-refresh-contents)))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(provide 'russell-package)

