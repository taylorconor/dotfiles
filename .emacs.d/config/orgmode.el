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
    ("p" "Project plan & todo list" plain
     (file (lambda () (ct-generate-plain-template "~/org/projects" "work, projects")))
     "%(format \"#+TITLE: %s\n#+STAMP: %s\n#+FILETAGS: %s\n\" ct-capture-name ct-capture-date ct-capture-tags)\n")))

;; make captures fullscreen, and make sure the previous buffer is restored on exit.
(defvar ct-org-capture-before-config nil
  "Window configuration before `org-capture'.")

(defadvice org-capture (before save-config activate)
  "Save the window configuration before `org-capture'."
  (setq ct-org-capture-before-config (current-window-configuration)))

(defun ct-org-capture-cleanup ()
  "Clean up the frame created while capturing via org-protocol."
  ;; in case we run capture from emacs itself and not an external app,
  ;; we want to restore the old window config
  (when ct-org-capture-before-config
    (set-window-configuration ct-org-capture-before-config))
  (-when-let ((&alist 'name name) (frame-parameters))
    (when (equal name "org-protocol-capture")
        (delete-frame))))

(add-hook 'org-capture-after-finalize-hook 'ct-org-capture-cleanup)
(add-hook 'org-capture-mode-hook 'delete-other-windows)

;; Make TODOs automatically reorder themselves upon state changes
(defun ct-org-sort-todo-list ()
  "Sort parent element in todo order."
  (org-up-element)
  (org-sort-entries nil ?o))

(add-hook 'org-after-todo-state-change-hook 'ct-org-sort-todo-list)

;;
;; Agenda settings.
;;
(setq org-agenda-files '("~/org/work.org"
                         "~/org/personal.org"
                         "~/org/autogen"
                         "~/org/meetings"
                         "~/org/meetings/1-1"
                         "~/org/projects"))
(setq org-agenda-span 30
      org-agenda-start-on-weekday nil)
;; try to force as many agenda windows fullscreen as possible.
(setq org-agenda-window-setup "only-window")
;; don't show reminders for upcoming deadlines.
(setq org-deadline-warning-days 0)

;;
;; Appearance.
;;
;; unfold everything on startup.
(setq org-startup-folded "content")
;; hide all starts except the rightmost one.
(setq org-hide-leading-stars t)
;; don't display DONE date/time after completing a todo.
(setq org-log-done 'nil)
;; pressing RET on a link follows it
(setq org-return-follows-link t)
;; following a link is fullscreen
(add-hook 'org-follow-link-hook 'delete-other-windows)
;; specify how sub-bullets are denoted
(setq org-list-demote-modify-bullet '(("1."  . "-")
                                      ("-"  . "-")))
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "CANCEL(c)")))
(setq org-todo-keyword-faces
 '(("TODO" . org-todo) ("WAIT" . "orange") ("DONE" . "green") ("CANCEL" . "green"))
 )
;; set sub-bullet color
'(org-level-3 ((t (:foreground "white"))))

;; don't add newlines before headings and list items.
(setf org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
;; don't consider newlines as content when folding.
(setq org-cycle-separator-lines -1)
;; TODO: this is a temporary hack to make list items in work.org more readable.
;; ideally we'd specify some contextual conditions under which we make the foreground white.
(custom-theme-set-faces 'user
                        `(org-level-3 ((t (:foreground "white")))))
;; Right-align tags
(setq org-tags-column (- 5 (window-width)))


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

;;
;; Google calendar integration
;;
(defun kill-url-browse-url-function (url &rest ignore)
  (kill-new url)
  (message "Killed: %s" url))
(unless window-system
  (setq  browse-url-browser-function 'kill-url-browse-url-function))
(require 'org-gcal)
(setq org-gcal-notify-p nil)
(setq org-gcal-client-id "xxx"
      org-gcal-client-secret "xxx"
      org-gcal-file-alist '(("xxx" .  "~/org/autogen/gcal.org")))
;; refresh the schedule file before rendering the agenda view
(add-hook 'org-agenda-mode-hook (lambda () (org-gcal-fetch) ))
;; disable prompt on event removal
(setq org-gcal-remove-api-cancelled-events t)
