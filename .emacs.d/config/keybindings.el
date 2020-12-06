;; clang-format
;;(require 'clang-format)
;;(global-set-key (kbd "M-=") 'clang-format-buffer)

;; emulate vim o and O
(defun smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(defun smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(global-set-key (kbd "M-o") 'smart-open-line)
(global-set-key (kbd "M-O") 'smart-open-line-above)

(global-set-key (kbd "C-x f") 'helm-find)

;; disable ctrl-z backgrounding shortcut
(global-unset-key (kbd "C-z"))
