;;; lsp.el --- LSP configuration -*- no-byte-compile: t; lexical-binding: t; -*-

(defun list-active-faces ()
  "List all faces used in the current buffer."
  (interactive)
  (let ((faces '()))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((face (get-text-property (point) 'face)))
          (when face
            (unless (listp face) (setq face (list face)))
            (dolist (f face)
              (unless (memq f faces)
                (push f faces)))))
        (forward-char 1)))
    (message "Faces in use: %s" faces)
    faces))

(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-level 4)
  :config
  (seq-do (lambda (it)
            (push it major-mode-remap-alist))
          '(
            (javascript-mode . js-ts-mode)
            (typescript-mode . typescript-ts-mode)
            (java-mode . java-ts-mode)
            (haskell-mode . haskell-ts-mode)
            (css-mode . css-ts-mode)
            (sh-mode . bash-ts-mode)
            (scala-mode . scala-ts-mode)
            (shell-script-mode . bash-ts-mode)))
  )

(use-package scala-ts-mode
  :ensure t
  :custom
  (scala-ts-indent-offset 2))

(use-package haskell-ts-mode
  :ensure t
  :custom
  (haskell-ts-use-indent t)
  (haskell-ts-ghci "ghci")
  :mode (("\\.hs\\'" . haskell-ts-mode)))

(use-package java-ts-mode
  :ensure t
  :mode (("\\.java\\") . java-ts-mode))

(use-package nix-ts-mode
  :mode "\\.nix\\'")

(use-package typescript-ts-mode
  :mode (("\\.cts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . typescript-ts-mode)
         ("\\.ts\\'" . typescript-ts-mode)))

(use-package eglot
  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)
  :config
  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
  ;; Disable eglot inlay hints by default
  (add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1)))

  ;; Add formatter to eglot-managed-mode-hook instead
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (when (eq major-mode 'scala-ts-mode)
                (add-hook 'before-save-hook #'eglot-format-buffer nil t))))
  
  (setq eglot-autoshutdown t))


;; Setup lsp for eglot for modes not currently supported by default by eglot
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(scala-ts-mode . ("metals")))
  (add-to-list 'eglot-server-programs
               '(haskell-ts-mode . ("cabal" "run" "lspipe" "--" "--server" "haskell-language-server --lsp" "--server" "typos-lsp" "--debug"))))

(define-prefix-command 'lsp-prefix-map)
(global-set-key (kbd "C-c l") 'lsp-prefix-map)

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
