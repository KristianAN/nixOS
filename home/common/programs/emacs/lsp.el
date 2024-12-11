;;; lsp.el --- LSP configuration -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-level 4)
  :config
  (seq-do (lambda (it)
            (push it major-mode-remap-alist))
          '((python-mode . python-ts-mode)
            (javascript-mode . js-ts-mode)
            (typescript-mode . typescript-ts-mode)
            (java-mode . java-ts-mode)
            (css-mode . css-ts-mode)
            (sh-mode . bash-ts-mode)
            (scala-mode . scala-ts-mode)
            (shell-script-mode . bash-ts-mode))))

(use-package scala-ts-mode
  :init
  (setq scala-ts-indent-offset 2))

(use-package haskell-mode)

(use-package nix-ts-mode
  :mode "\\.nix\\'")

(use-package eglot-booster
  :after eglot
  :config	(eglot-booster-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(scala-ts-mode . ("metals"))))

(use-package eldoc-box)