;; hide the menubar
(menu-bar-mode -1)
;; hide the startup screen
(setq inhibit-startup-message t)
;; empty the scratch buffer
(setq initial-scratch-message "")

;; highlight current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "#333333")
(set-face-foreground 'highlight nil)

;; enable line numbers for all files
(global-linum-mode t)

;; dynamic, right-aligned line numbers
(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat "%" (number-to-string w) "d ")))
    ad-do-it))

;; Enable syntax highlighting
(global-font-lock-mode 1)

;; Monokai theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'monokai t)

;; Add line and col numbers to modeline
(column-number-mode 1)
(line-number-mode 1)

;; smart-line-mode settings
(setq sml/show-encoding nil)
(setq sml/theme 'dark)
(setq sml/no-confirm-load-theme t)
(sml/setup)
