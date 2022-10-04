(require 'google)
(require 'google3-mode)
(require 'google-cc-extras)
(require 'google-trailing-whitespace)
(require 'google-cc-add-using)
(require 'google3-display-citc-client)
(require 'google3-build-cleaner)
(require 'google-imports)
(require 'google-critique)
(require 'google-findings)

;; eglot code completion
(require 'google3-eglot)
(setq eglot-sync-connect 0)
(google3-eglot-setup)

;; additional code completion settings
(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 0)

;; format on save.
(add-hook 'c++-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'google-clang-format-file nil :local)))

;; rotate between file extensions
(require 'rotate-among-files)
(global-set-key (kbd "C-c b") #'google-rotate-among-files)

;; blaze integration
;; (require 'google3-build)
;; (require 'compilation-colorization)
;; (define-key google3-mode-map (kbd "C-c b") #'google3-build)

;; enable company-mode for all files
(add-hook 'after-init-hook 'global-company-mode)

;; build files
(require 'google3-build-capf)
(google3-build-capf-enable-completions)
(add-to-list 'company-backends #'company-capf)

;; go/yasnippets
(require 'google-yasnippets)
(google-yasnippets-load)
(yas-global-mode 1)

;; codesearch
(require 'csearch)
(require 'ivy-cs)
(global-set-key (kbd "C-x f") #'ivy-cs)

;; commit diffs
(global-git-gutter-mode t)
'(git-gutter:handled-backends '(hg git g4))

;; auto-save
(require 'real-auto-save)
(add-hook 'prog-mode-hook 'real-auto-save-mode)
(setq real-auto-save-interval 5)
