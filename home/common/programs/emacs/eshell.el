;;; eshell.el --- Eshell customizations -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package eat
  :hook ((eshell-load . eat-eshell-mode))
  :bind
  (("C-c t e" . eat)
   ("C-c t p" . eat-project)
   ("C-c t o" . eat-project-other-window)))
