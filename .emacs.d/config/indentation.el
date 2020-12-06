;; stop emacs from indenting inside a namespace scope
(defun my-c-setup ()
   (c-set-offset 'innamespace [0]))
(add-hook 'c++-mode-hook 'my-c-setup)
