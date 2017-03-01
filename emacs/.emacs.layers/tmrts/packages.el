(defun tmrts/post-init-org ()
  "Configure org mode"
  (use-package org
    :config
    (progn
      (setq
       org-hide-emphasis-markers t
       org-pretty-entities t
       org-startup-indented t
       org-startup-with-inline-images t
       org-startup-with-latex-preview t
       org-bullets-bullet-list '("◉" "○" "✸" "◆" "▶")
       org-todo-keywords '("TODO(t)" "WORKING(w!)"
                           "|"
                           "DONE(d!)" "CANCELED(c@)")))))

(defun tmrts/post-init-org-agenda ()
  (use-package org-agenda
    :config
    (progn
      (add-to-list 'org-agenda-files "~/org/"))))

(defun tmrts/init-org-drill ()
  "Configure org-drill"
  (use-package org-drill
    :init
    (progn
      (setq
       org-drill-learn-fraction 0.3
       org-drill-add-random-noise-to-intervals-p t
       org-drill-adjust-intervals-for-early-and-late-repetitions-p t
       org-drill-save-buffers-after-drill-sessions-p nil))))

(defun tmrts/post-init-org-present()
  "Configure org-present"
  (use-package org-present
    :config
    (progn)))

(defun tmrts/init-helm-org-rifle ()
  "Configure helm-org-rifle"
  (use-package helm-org-rifle
    :commands (helm-org-rifle)
    :bind (("M-i" . helm-org-rifle))
    :config
    (progn)))

(defun tmrts/init-org-capture ()
  "Configure org-capture"
  (use-package org-capture
    :config
    (progn
      (add-to-list 'org-capture-templates
                   '("j" "Journal Entry" entry (file+datetree "~/org/journal.org")
                     "* %^{Title}\n%U\n\n%?\n"))
      (add-to-list 'org-capture-templates
                   '("c" "Clock" entry (file+datetree "~/org/timesheet.org")
                     "* %U - %^{Activity} :TIME:"))
      (add-to-list 'org-capture-templates
                   '("t" "TODO" entry (file "~/org/outline/todo.org")
                     "* TODO %?\n SCHEDULED: %^t")))))

(defconst tmrts-packages
  '(helm-org-rifle
    (org :location built-in)
    (org-agenda :location built-in)
    (org-capture :location built-in)
    (org-drill :location built-in)
    org-present))

;;; packages.el ends here
