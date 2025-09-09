;;; eat.el --- Terminal and eShell solution -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package eat
  :bind
  (("C-c t p" . eat-project-other-window)
   ("C-c t n" . eat)
   ("C-c t s" . eat-project)
   ("C-c t g" . kristian/consult-grep-project-buffer-term)
   ))

