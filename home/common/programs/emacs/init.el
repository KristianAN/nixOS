;;; init --- Extra configuration for emacs in nix
;;; Commentary:

;;; Code:
;; (setq inhibit-startup-message t) ; Disable startup message
(menu-bar-mode -1) ; Disable the menu bar
(tool-bar-mode -1) ; Disable the toolbar
(scroll-bar-mode -1) ; Disable the scroll bar
(setq-default indent-tabs-mode nil) ; Use spaces instead of tabs

;; Load packages
(load-file "~/nix/nixOS/home/common/programs/emacs/evil-config.el")
(load-file "~/nix/nixOS/home/common/programs/emacs/vterm-config.el")
(load-file "~/nix/nixOS/home/common/programs/emacs/tabspaces-config.el")
;;(load-file "~/nix/nixOS/home/common/programs/emacs/eglot-config.el")
(load-file "~/nix/nixOS/home/common/programs/emacs/indent-config.el")
(load-file "~/nix/nixOS/home/common/programs/emacs/web-config.el")
(load-file "~/nix/nixOS/home/common/programs/emacs/org-roam-config.el")
(load-file "~/nix/nixOS/home/common/programs/emacs/font-and-theme.el")
(load-file "~/nix/nixOS/home/common/programs/emacs/completion.el")
(load-file "~/nix/nixOS/home/common/programs/emacs/lsp-mode-config.el")

;; Not working plugins that I want to make work
;;(load-file "~/nix/nixOS/home/common/programs/emacs/scala-cli-repl-config.el")
;;(load-file "~/nix/nixOS/home/common/programs/emacs/dape-config.el")

;; General Settings
(setq make-backup-files nil) ; Disable backup files
(setq auto-save-default nil) ; Disable auto save
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq lsp-idle-delay 0.500)

;; magit-forge
(use-package forge
  :after magit)

;; Turn off bell
(setq ring-bell-function 'ignore)

;; line-numbers
(setq display-line-numbers-type 'default)
( global-display-line-numbers-mode t)

;; Rg
(require 'rg)

;; Flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; Set keybindings for moving between windows
(global-set-key (kbd "C-h") 'windmove-left)
(global-set-key (kbd "C-j") 'windmove-down)
(global-set-key (kbd "C-k") 'windmove-up)
(global-set-key (kbd "C-l") 'windmove-right)

;; Which Key
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package sql
  :ensure t
)

;; Direnv Configuration
(direnv-mode)

;; EditorConfig
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; Some hooks
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(server-start)
;;; init.el ends here
