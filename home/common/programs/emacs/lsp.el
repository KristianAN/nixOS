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
            (haskell-mode . haskell-ts-mode)
            (css-mode . css-ts-mode)
            (sh-mode . bash-ts-mode)
            (scala-mode . scala-ts-mode)
            (shell-script-mode . bash-ts-mode))))

(use-package scala-ts-mode
  :init
  (setq scala-ts-indent-offset 2))

(use-package haskell-ts-mode
  :mode (("\\.hs\\'" . haskell-ts-mode)
         ("\\.cabal\\'" . haskell-ts-mode)))

(use-package nix-ts-mode
  :mode "\\.nix\\'")

(use-package typescript-ts-mode
  :mode (("\\.cts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . typescript-ts-mode)
         ("\\.ts\\'" . typescript-ts-mode)))

(use-package python-ts-mode
  :mode ("\\.py\\'" . python-ts-mode))

;; Package that helps with eglot performance
(use-package eglot-booster
  :after eglot
  :config	(eglot-booster-mode))

;; Disable eglot inlay hints by default
(add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1)))

;; Setup lsp for eglot for modes not currently supported by default by eglot
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(scala-ts-mode . ("metals")))
  (add-to-list 'eglot-server-programs
               '(haskell-ts-mode . ("haskell-language-server-wrapper" "--lsp"))))

(define-prefix-command 'lsp-prefix-map)
(global-set-key (kbd "C-l") 'lsp-prefix-map)

(dolist (binding '(("l" . eglot)
                   ("a" . eglot-code-actions)
                   ("f" . eglot-format-buffer)
                   ("r" . eglot-rename)
                   ("H" . eglot-inlay-hints-mode)
                   ("d" . eglot-find-declaration)
                   ("i" . eglot-find-implementation)
                   ("t" . eglot-find-typeDefinition)
                   ("e" . flymake-goto-next-error)
                   ("o" . eglot-organize-imports)
                   ("h" . eldoc-box-help-at-point)))

  (define-key lsp-prefix-map (kbd (car binding)) (cdr binding)))

(use-package eldoc-box)

(use-package web-mode
  :ensure t
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.php\\'" . web-mode)
   ("\\.vue\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)))
