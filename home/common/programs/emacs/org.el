;;; org.el --- org-mode -*- no-byte-compile: t; lexical-binding: t; -*-

;; active Babel languages
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((haskell . t)
;;    (emacs-lisp . t)))

(use-package org
  :ensure nil
  :custom
  ((org-export-backends (quote (ascii html icalendar latex md odt))))
  )

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

