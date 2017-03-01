;; (defun tmrts/checkbox-list-complete ()
;;   "If the heading is marked TODO, transition to DONE when all
;; checkboxes are marked."
;;   (save-excursion
;;     (org-back-to-heading t)
;;     (let ((beg (point)) end)
;;       (end-of-line)
;;       (setq end (point))
;;       (goto-char beg)
;;       (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]" end t)
;;           (if (match-end 1)
;;               (if (equal (match-string 1) "100%")
;;                   ;; all done - do the state change
;;                   (org-todo 'done)
;;                 (org-todo 'todo))
;;             (if (and (> (match-end 2) (match-beginning 2))
;;                      (equal (match-string 2) (match-string 3)))
;;                 (org-todo 'done)
;;               (org-todo 'todo)))))))

;; (defun tmrts/current-file-name ()
;;   (buffer-file-name (current-buffer)))

;; (defvar tmrts/drill-file-extension ".drill.org")

;; (defun string/ends-with (string suffix)
;;   (and (string-match (rx-to-string `(: ,suffix eos) t)
;;                      string)
;;        t))

;; (defun tmrts/drill-file-name (file-name)
;;    (if (string/ends-with file-name tmrts/drill-file-extension)
;;        file-name
;;      (replace-regexp-in-string "\\.org" tmrts/drill-file-extension file-name)))

;; (defun tmrts/find-drill-file ()
;;   (find-file
;;    (tmrts/drill-file-name (tmrts/current-file-name))))

;; (defun org-drill-present-topic ()
;;   (with-hidden-comments
;;    (with-hidden-cloze-hints
;;     (with-hidden-cloze-text
;;      (org-drill-hide-all-subheadings-except nil)
;;      ;(ignore-errors
;;        ;(org-drill--show-latex-fragments)
;;        ;(org-display-inline-images t))
;;      (org-cycle-hide-drawers 'all)
;;      (prog1 (org-drill-presentation-prompt)
;;        (org-drill-hide-subheadings-if 'org-drill-entry-p))))))

;; (defun org-drill-show-answer-topic (reschedule-fn)
;;   (org-cycle-hide-drawers 'all)
;;   ;(org-drill-goto-drill-entry-heading)
;;   (let ((topic-id (org-entry-get (point) "TOPIC_ID"))
;;         (topic-file-name
;;          (concat
;;           (string-remove-suffix
;;            tmrts/drill-file-extension
;;            (tmrts/current-file-name))
;;           ".org")))
;;     (save-window-excursion
;;       (split-window-right-and-focus)
;;       ;(switch-to-buffer-other-window "drill-answer")
;;       (org-open-file topic-file-name)
;;       (org-set-startup-visibility)
;;       ; TODO: Handle search fails
;;       (goto-char (cdr (org-id-find topic-id)))
;;       (org-cycle)
;;       (org-narrow-to-subtree)
;;       (windmove-right)
;;       ; TODO: Close buffers after the topic is done?
;;       (funcall reschedule-fn))))

;; (defun org-drill-capture-topic ()
;;   (interactive)
;;   (save-excursion)
;;   )

;; (defun org-drill-capture-topic ()
;;   (interactive)
;;   (require 'org-capture)
;;   (save-excursion
;;     ; (setq tmrts/org-drill-current-target-file-name (tmrts/drill-file-name (tmrts/current-file-name)))
;;     (let ((topic-id (org-id-get-create)))
;;       (org-capture nil "d")
;;                                         ; TODO: use before-finalize hook
;;       (org-set-property "DRILL_CARD_TYPE" "topic")
;;       (org-set-property "TOPIC_ID" topic-id)
;;       (org-toggle-tag "drill"))))
