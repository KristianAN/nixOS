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
  :init
  (spacious-padding-mode 1))

(use-package breadcrumb
  :ensure t
  :init
  (breadcrumb-mode)
  )

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

(use-package ef-themes
  :ensure t
  :init
  (ef-themes-take-over-modus-themes-mode 1)
  :bind
  (("<f5>" . modus-themes-rotate)
   ("C-<f5>" . modus-themes-select)
   ("M-<f5>" . modus-themes-load-random))
  :config
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-italic-constructs t)
  (setq modus-themes-to-toggle '(ef-night ef-day))

  (set-face-attribute 'default nil :family "Aporetic Sans Mono" :height 135)
  (set-face-attribute 'variable-pitch nil :family "Aporetic Sans" :height 1.0)
  (set-face-attribute 'fixed-pitch nil :family "Aporetic Sans Mono" :height 1.0)

  (modus-themes-load-theme 'ef-night))
