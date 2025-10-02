;;; looks.el --- Customize Emacs UI looks -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Line numbers
(setq display-line-numbers-type 'relative) 
(global-display-line-numbers-mode)

;;; Icons
(use-package nerd-icons)

(use-package spacious-padding
  :config
  (setq spacious-padding-widths
        '( :internal-border-width 15
           :header-line-width 4
           :mode-line-width 6
           :tab-bar-width 4
           :tab-width 4
           :right-divider-width 30
           :scroll-bar-width 0
           :fringe-width 8)
        )
  :custom
  (spacious-padding-mode 1))

;; Pretty icons for completion
(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-marginalia-setup)
  (nerd-icons-completion-mode 1))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Setup modus-themes if we don't want to use any package for themes

(require-theme 'modus-themes) 

(setq modus-themes-to-toggle '(modus-vivendi modus-operandi))

(set-face-attribute 'default nil :family "Aporetic Sans Mono" :height 130)
(set-face-attribute 'variable-pitch nil :family "Aporetic Sans" :height 1.0)
(set-face-attribute 'fixed-pitch nil :family "Aporetic Sans Mono" :height 1.0)

(load-theme 'modus-vivendi)
