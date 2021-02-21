;; configure backups
(setq backup-directory-alist `(("." . "~/.emacs_backups/")))
(setq backup-by-copying t)
(setq delete-old-versions t)

;; follow symlinks
(setq vc-follow-symlinks t)

;; ido mode is fundamental
(require 'ido-vertical-mode)
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
