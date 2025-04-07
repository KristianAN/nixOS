;;; dired.el --- polished dired -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package dired
  :ensure nil  ; dired is built-in, no need to ensure
  :config
  (define-key dired-mode-map (kbd "e") 'dired-previous-line)  ; Bind 'e' to move up
  :bind (:map dired-mode-map
              ("e" . dired-previous-line)))  ; Alternative binding style
