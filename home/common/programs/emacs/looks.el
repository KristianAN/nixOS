;;; looks.el --- Customize Emacs UI looks -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Line numbers
(setq display-line-numbers-type 'relative) 
(global-display-line-numbers-mode)

;;; Icons
(use-package nerd-icons)

(use-package doric-themes
  :ensure t
  :demand t
  :config
  ;; These are the default values.
  (setq doric-themes-to-toggle '(doric-dark doric-light))
  (setq doric-themes-to-rotate doric-themes-collection)

  (doric-themes-select 'doric-dark)

  (set-face-attribute 'default nil :family "Aporetic Sans Mono" :height 130)
  (set-face-attribute 'variable-pitch nil :family "Aporetic Sans" :height 1.0)
  (set-face-attribute 'fixed-pitch nil :family "Aporetic Sans Mono" :height 1.0))


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
