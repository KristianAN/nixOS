;;; vterm-config --- configuration for vterm and multi-vterm
;;; Commentary:
;;; Code:
;; vterm
(use-package vterm
  :ensure t)

(use-package multi-vterm
  :ensure t
  :after vterm
  :config
  (setq multi-vterm-dedicated-window-height 50))
;;; vterm-config.el ends here
