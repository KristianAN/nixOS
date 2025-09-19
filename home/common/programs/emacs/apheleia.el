;;; apheleia.el --- Formatting configuration -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package apheleia
  :ensure t
  :config
  (progn
    (setq apheleia-formatters
          (seq-concatenate
           'list
           '(
             ;; google-java-format reads from stdin when passed "-" and writes formatted output to stdout.
             (google-java-format . ("google-java-format" "-"))
             (cabal-gild . ("cabal-gild"))
             (fourmolu . ("fourmolu" "--stdin-input-file" filepath))
             (prettier . ("npx" "prettier" "--stdin-filepath" filepath))
             )
           apheleia-formatters))

    (setq apheleia-mode-alist
          (seq-concatenate
           'list
           '((haskell-mode . fourmolu)
             (haskell-cabal-mode . cabal-gild)
             (web-mode . prettier)
             (typescript-ts-mode . prettier)
             ;; Java modes -> google-java-format
             (java-mode . google-java-format)
             (java-ts-mode . google-java-format)
             )
           apheleia-mode-alist))

    ;; Disable apheleia for scala-ts-mode to let eglot handle formatting
    (add-hook 'scala-ts-mode-hook
              (lambda () (apheleia-mode -1)))))
(apheleia-global-mode +1)
