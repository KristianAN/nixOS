;;; eglot-config --- Configuration for eglot
;;; Commentary:
;;; Code:
(setq major-mode-remap-alist
'((yaml-mode . yaml-ts-mode)
    (bash-mode . bash-ts-mode)
    (js2-mode . js-ts-mode)
    (typescript-mode . typescript-ts-mode)
    (json-mode . json-ts-mode)
    (css-mode . css-ts-mode)
    (python-mode . python-ts-mode)))

;; Tree-sitter-config
(setq treesit-font-lock-level 4)

;; Eglot
(dolist (mode
    '(
        clojure-mode-hook
        clojurescript-mode-hook
        clojurec-mode-hook
        scala-ts-mode-hook
        ))
(add-hook mode 'eglot-ensure))
(add-hook 'java-mode-hook 'eglot-java-mode)
(add-hook 'after-save-hook
          (lambda ()
            (when (eglot-managed-p)
              (eglot-format))))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '((scala-mode scala-ts-mode) . ("metals")))
  (add-to-list 'eglot-server-programs '((clojure-mode clojure-ts-mode) . ("clojure-lsp"))))
;;; eglot-config.el ends here

