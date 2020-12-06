;; configure backups
(setq backup-directory-alist `(("." . "~/.emacs_backups/")))
(setq backup-by-copying t)
(setq delete-old-versions t)

;; follow symlinks
(setq vc-follow-symlinks t)
