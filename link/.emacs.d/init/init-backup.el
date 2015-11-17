;; Backup settings

;; Don't create ~ files when editing
; (setq create-lockfiles nil)

(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
    (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t              ; backup of a file the first time it is saved
      ; backup-by-copying t
      backup-by-copying-when-linked t
      version-control t                ; version control for backup files
      delete-old-versions t            ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6              ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9              ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t              ; auto-save every buffer that visits a file
      auto-save-timeout 20             ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200           ; number of keystrokes between auto-saves (default: 300)
      )

(defvar --autosave-directory (concat user-emacs-directory "auto-saves"))
(if (not (file-exists-p --autosave-directory))
    (make-directory --autosave-directory t))
(setq auto-save-file-name-transforms `((".*" ,--autosave-directory t)))


(provide 'init-backup)
