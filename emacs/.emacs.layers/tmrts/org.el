;;; packages.el --- tmrts layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Tamer Tas
;;
;; Author:  <tmrts@forge>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun tmrts/post-init-org ()
  "Configure org mode"
  (use-package org
    :config (progn
              (setq
               org-hide-emphasis-markers t
               org-pretty-entities t
               org-startup-indented t
               org-startup-with-inline-images t
               org-startup-with-latex-preview t
               org-agenda-files '("~/org")))))
