(require 'org)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;;
;; Basic keybindings.
;;
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(setq org-log-done t)

;;
;; Capture templates.
;;
(defun ct-generate-plain-template (path initial)
  (setq ct-capture-name (read-string "Name: "))
  (setq ct-capture-date (read-string "Date: " (format-time-string "%Y-%m-%d")))
  (setq ct-capture-tags (concat ":" (replace-regexp-in-string ", " ":" (read-string "Tags: " initial)) ":"))
  (expand-file-name (format "%s-%s.org" ct-capture-date ct-capture-name) path))

(defun ct-create-new-meeting-instance (path)
  (setq ct-capture-name (read-file-name "File: " path))
  (setq ct-capture-date (read-string "Date: " (format-time-string "%Y-%m-%d")))
  (find-file ct-capture-name)
  (org-capture-finalize)
  ;; TODO: insert some text in the buffer to add a new section
  )

(setq org-capture-templates
  '(("m" "Meeting note" plain
     (file (lambda () (ct-generate-plain-template "~/org/meetings" "work, meetings")))
     "%(format \"#+TITLE: %s\n#+STAMP: %s\n#+FILETAGS: %s\n\" ct-capture-name ct-capture-date ct-capture-tags)\n* Agenda\n- [ ] \n\n* Notes\n\n* Follow-ups\n")
    ("o" "1-1 meeting note (w/ reporting chain)" plain
     (file (lambda () (ct-generate-plain-template "~/org/meetings/1-1" "work, meetings, 1-1")))
     "%(format \"#+TITLE: %s\n#+STAMP: %s\n#+FILETAGS: %s\n\" ct-capture-name ct-capture-date ct-capture-tags)\n* Agenda\n- [ ] \n\n* Notes\n\n* Follow-ups\n")
    ("p" "Project plan & todo list" plain
     (file (lambda () (ct-generate-plain-template "~/org/projects" "work, projects")))
     "%(format \"#+TITLE: %s\n#+STAMP: %s\n#+FILETAGS: %s\n\" ct-capture-name ct-capture-date ct-capture-tags)\n")
    ("i" "Instance of a recurring meeting" item
     (function (lambda () (ct-create-new-meeting-instance "~/org/meetings/recurring/"))))))

;; Make captures fullscreen, and make sure the previous buffer is restored on exit.
(defvar ct-org-capture-before-config nil
  "Window configuration before `org-capture'.")

(defadvice org-capture (before save-config activate)
  "Save the window configuration before `org-capture'."
  (setq ct-org-capture-before-config (current-window-configuration)))

(defun ct-org-capture-cleanup ()
  "Clean up the frame created while capturing via org-protocol."
  ;; In case we run capture from emacs itself and not an external app,
  ;; we want to restore the old window config
  (when ct-org-capture-before-config
    (set-window-configuration ct-org-capture-before-config))
  (-when-let ((&alist 'name name) (frame-parameters))
    (when (equal name "org-protocol-capture")
        (delete-frame))))

;;(add-hook 'org-capture-after-finalize-hook 'ct-org-capture-cleanup)
(add-hook 'org-capture-mode-hook 'delete-other-windows)


;;
;; Agenda settings.
;;
(setq org-agenda-files (list "~/org/weekly.org"
                             "~/org/meetings"
                             "~/org/meetings/1-1"
                             "~/org/projects"))
(setq org-agenda-span 30
      org-agenda-start-on-weekday nil
      org-agenda-start-day "-3d")
(setq org-agenda-window-setup "only-window") ; try to force as many agenda windows fullscreen as possible
(setq org-deadline-warning-days 0) ; don't show reminders for upcoming deadlines.

;;
;; Appearance.
;;
(setq org-startup-folded "content") ; unfold everything on startup.
(setq org-hide-leading-stars t) ; hide all starts except the rightmost one.
(setq org-log-done 'nil) ; don't display DONE date/time after completing a todo
(setq org-return-follows-link t) ; pressing RET on a link follows it
(add-hook 'org-follow-link-hook 'delete-other-windows) ; following a link is fullscreen
; Specify how sub-bullets are demoted
(setq org-list-demote-modify-bullet '(("1."  . "-")
                                      ("-"  . "-")))
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))
(setf org-blank-before-new-entry '((heading . nil) (plain-list-item . nil))) ; don't add newlines before headings and list items.
(setq org-cycle-separator-lines -1) ; don't consider newlines as content when folding.

;;
;; Deft setup for easy org file searching.
;;
(require 'deft)
(setq deft-default-extension "org")
(setq deft-extensions '("org"))
(setq deft-directory "~/org")
(setq deft-recursive t)
(setq deft-use-filename-as-title nil)
(setq deft-use-filter-string-for-filename t)
(setq deft-text-mode 'org-mode)
(setq deft-current-sort-method 'mtime)
(global-set-key (kbd "\C-cd") 'deft)
(global-set-key (kbd "\C-cf") 'deft-find-file)
