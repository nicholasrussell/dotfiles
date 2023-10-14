;;; early-init.el -*- lexical-binding: t; -*-

;;; Set up packages
(require 'package)
(require 'time-date)
(require 'seq)

(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(customize-set-variable 'package-archive-priorities
                        '(("gnu" . 99)
                          ("nongnu" . 90)
                          ("melpa-stable" . 80)
                          ("melpa" . 0)))

(defun russell/package-archives-stale-p ()
  (interactive)
  (cl-some
   (lambda (archive)
     (let* ((today (decode-time nil nil t))
            (archive-name (expand-file-name
                           (format "archives/%s/archive-contents" archive)
                           package-user-dir))
            (last-update-time (decode-time (file-attribute-modification-time
                                            (file-attributes archive-name))))
            (delta (make-decoded-time :day 1))) ;; max 1 day stale
       (time-less-p (encode-time (decoded-time-add last-update-time delta))
                    (encode-time today))))
   (mapcar #'car package-archives)))

(defun russell/package-initialize ()
  (when package-enable-at-startup
    (package-initialize)

    (cond ((seq-empty-p package-archive-contents)
           (progn
             (message "russell/package: Package archives empty, initializing")
             (package-refresh-contents)))
          ((russell/package-archives-stale-p)
           (progn
             (message "russell/package: Package archives stale, refreshing")
             (package-refresh-contents t))))
    (message "russell/package: Package system initialized")))

(add-hook 'before-init-hook #'russell/package-initialize)

(provide 'early-init)

