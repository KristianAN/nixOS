;;; org-roam-config.el --- org-roam configuration
;;; Commentary:
;;; Code:
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-directory (file-truename "~/org-roam"))
  :config
  (org-roam-db-autosync-mode))

(use-package org-modern
  :ensure t
  :config
  (with-eval-after-load 'org (global-org-modern-mode)))
;;; org-roam-config.el ends here
