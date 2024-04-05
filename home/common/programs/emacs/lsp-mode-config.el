;;; lsp-mode-config --- Configuration for lsp-mode
;;; Commentary:
;;; Code:
(setq major-mode-remap-alist
'((yaml-mode . yaml-ts-mode)
    (bash-mode . bash-ts-mode)
    (js-mode . js-ts-mode)
    (typescript-mode . typescript-ts-mode)
    (json-mode . json-ts-mode)
    (java-mode . java-ts-mode)
    (scala-mode . scala-ts-mode)
    (css-mode . css-ts-mode)
    (python-mode . python-ts-mode)))

(use-package lsp-metals
  :ensure t
  :config
  (setq lsp-metals-server-command "metals"))


(use-package yasnippet
  :ensure t
  )

(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)

(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (scala-ts-mode . lsp)
         (java-ts-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (setq lsp-enable-suggest-server-download nil))

(use-package lsp-ui)
;;; lsp-mode-config.el ends here
