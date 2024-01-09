;;; eglot-config --- Configuration for eglot
;;; Commentary:
;;; Code:
(setq major-mode-remap-alist
'((yaml-mode . yaml-ts-mode)
    (bash-mode . bash-ts-mode)
    (js-mode . js-ts-mode)
    (typescript-mode . typescript-ts-mode)
    (json-mode . json-ts-mode)
    (java-mode . java-ts-mode)
    (css-mode . css-ts-mode)
    (python-mode . python-ts-mode)))

;; Tree-sitter-config
(setq treesit-font-lock-level 4)

;; Eglot
(use-package eglot
  :ensure t
  :hook ((clojure-mode . eglot-ensure)
         (clojurescript-mode . eglot-ensure)
         (clojurec-mode . eglot-ensure)
         (scala-ts-mode . eglot-ensure)
         (java-mode . eglot-java-mode)
         (java-ts-mode . eglot-java-mode)
         (after-save . (lambda ()
                         (when (and (fboundp 'eglot-managed-p) (eglot-managed-p))
                           (eglot-format)))))
  :config
  (add-to-list 'eglot-server-programs '((scala-mode scala-ts-mode) . ("metals")))
  (add-to-list 'eglot-server-programs '((typescript-mode typescript-ts-mode js-ts-mode) . ("typescript-language-server")))
  (add-to-list 'eglot-server-programs '((clojure-mode clojure-ts-mode) . ("clojure-lsp"))))
;;; eglot-config.el ends here

