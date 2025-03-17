(use-package org-roam)

(define-prefix-command 'org-prefix-map)

(global-set-key (kbd "C-x o") 'org-prefix-map)

(define-key org-prefix-map (kbd "c") 'org-roam-capture)

(setq org-roam-directory (file-truename "~/org-roam"))

(org-roam-db-autosync-mode)

(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
