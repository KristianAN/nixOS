;;; web-config --- Configuration for general web development
;;; Commentary:
;;; Code:

;; Javascript Environment
(helm-mode +1)
(require 'web-mode)
(require 'helm-xref)
(require 'vue-mode)
(require 'json-mode)
(require 'dap-chrome)
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; Web-mode configurations
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.liquid\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(setq web-mode-enable-auto-closing t)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-part-padding 2)
(setq web-mode-script-padding 2)
(setq web-mode-style-padding 2)
(setq web-mode-code-indent-offset 2)
;;; web-config.el ends here
