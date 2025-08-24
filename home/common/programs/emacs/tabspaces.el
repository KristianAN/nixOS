;;; tabspaces.el --- LSP configuration -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package tabspaces
  :bind
  (("C-x p o" . tabspaces-open-or-create-project-and-workspace)
   ("C-x p p" . tabspaces-project-switch-project-open-file)))
