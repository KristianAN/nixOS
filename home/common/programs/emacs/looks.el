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
  :config
  (defun my/hide-tab-bar-when-single (&rest _)
    "Hide the tab bar if only one tab is open; show it otherwise."
    (if (<= (length (tab-bar-tabs)) 1)
        (tab-bar-mode -1)  ;; Turn off the tab-bar itself, not vim-tab-bar-mode
      (unless tab-bar-mode
        (tab-bar-mode 1))))
  
  (advice-add 'tab-bar-new-tab :after #'my/hide-tab-bar-when-single)
  (advice-add 'tab-bar-close-tab :after #'my/hide-tab-bar-when-single)
  (advice-add 'tab-bar-select-tab :after #'my/hide-tab-bar-when-single)
  
  :hook
  (after-init . (lambda ()
                  (vim-tab-bar-mode 1)
                  (my/hide-tab-bar-when-single))))

