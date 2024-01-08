;;; tabspaces-config --- Configuration for tabspaces
;;; Commentary:
;;; Code:
(use-package tabspaces
  :ensure t
  :init
  (setq
    tabspaces-use-filtered-buffers-as-default t
    tabspaces-default-tab "Default"
    tabspaces-remove-to-default t
    tabspaces-include-buffers '("*scratch*")
    tabspaces-initialize-project-with-todo t
    tabspaces-todo-file-name "project-todo.org"
    tabspaces-session nil
    tabspaces-session-auto-restore nil)
  :config
  (add-hook 'prog-mode-hook #'tabspaces-mode))
;;; tabspaces-config.el ends here

