;;; web-config --- Configuration for general web development
;;; Commentary:
;;; Code:

;; Javascript Environment
(use-package web-mode
  :ensure t
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.liquid\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html?\\'" . web-mode))
  :config
  (setq web-mode-enable-auto-closing t
        web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-part-padding 2
        web-mode-script-padding 2
        web-mode-style-padding 2
        web-mode-code-indent-offset 2))

(use-package vue-mode
  :ensure t
  :mode ("\\.vue\\'" . vue-mode))

(use-package json-mode
  :ensure t)
;;; web-config.el ends here
