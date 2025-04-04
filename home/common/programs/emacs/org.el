;;; org.el --- org-mode -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package org-roam)

(define-prefix-command 'org-prefix-map)

(setq org-roam-directory (file-truename "~/org-roam"))

(org-roam-db-autosync-mode)

(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

;; active Babel languages
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((mermaid . t)
;;    (emacs-lisp . t)))

