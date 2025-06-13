;;; apheleia.el --- Formatting configuration -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package apheleia
  :ensure t
  :config
  (progn
    (setq apheleia-formatters
          (seq-concatenate
           'list
           '(
             (cabal-gild . ("cabal-gild"))
             (fourmolu . ("fourmolu" "--stdin-input-file" filepath))
             (scalafmt . ("scalafmt" filepath "--stdin" "--non-interactive" "--quiet" "--stdout"))
             (prettier . ("npx"  "prettier" "--stdin-filepath" filepath))
             )
           apheleia-formatters))

    (setq apheleia-mode-alist
          (seq-concatenate
           'list
           '((haskell-mode . fourmolu)
             (haskell-cabal-mode . cabal-gild)
             (scala-ts-mode . scalafmt)
             (web-mode . prettier)
             (typescript-ts-mode . prettier)
             )
           apheleia-mode-alist))
    )
  )

(apheleia-global-mode +1)
