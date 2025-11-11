;;; org.el --- org-mode -*- no-byte-compile: t; lexical-binding: t; -*-

;; active Babel languages
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((haskell . t)
;;    (emacs-lisp . t)))

(use-package org
  :ensure nil
  :config
  (setq org-M-RET-may-split-line '((default . nil)))
  (setq org-insert-heading-respect-content t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-directory "~/org/")
  (setq org-agenda-files (list org-directory))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAIT(w!)" "|" "CANCEL(c!)" "DONE(d!)")))
  :custom
  ((org-export-backends (quote (ascii html icalendar latex md odt))))
  )

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

