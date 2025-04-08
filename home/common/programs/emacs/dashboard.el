;;; dashboard.el --- A pretty dashboard -*- no-byte-compile: t; lexical-binding: t; -*-


;; A startup screen extracted from Spacemacs
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  :custom
  (initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
  (dashboard-projects-backend 'project-el)
  (dashboard-banner-logo-title nil)
  (dashboard-center-content t)
  (dashboard-set-footer nil)
  (dashboard-page-separator "\n\n")
  (dashboard-items '((projects . 10)
                     (recents  . 10)
                     )))

