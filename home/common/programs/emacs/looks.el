;;; looks.el --- Customize Emacs UI looks -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Line numbers
(setq display-line-numbers-type 'relative) 
(global-display-line-numbers-mode)

;;; Icons
(use-package nerd-icons)

;;; Set font
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              ;; All customizations here
              (load-theme 'ef-night t)
              (set-frame-font "Aporetic Sans Mono 12" nil t)
              )))


(use-package vim-tab-bar
  :ensure t
  :commands vim-tab-bar-mode
  :hook
  (after-init . vim-tab-bar-mode))
