;;; init --- Extra configuration for emacs in nix
;;; Commentary:

;;; Code:
(setq inhibit-startup-message t) ; Disable startup message
(menu-bar-mode -1) ; Disable the menu bar
(tool-bar-mode -1) ; Disable the toolbar
(scroll-bar-mode -1) ; Disable the scroll bar
(setq-default indent-tabs-mode nil) ; Use spaces instead of tabs

;; Load packages
(load-file "~/nix/nixOS/home/common/programs/emacs/evil-config.el")
(load-file "~/nix/nixOS/home/common/programs/emacs/vterm-config.el")
(load-file "~/nix/nixOS/home/common/programs/emacs/git-gutter-config.el")
(load-file "~/nix/nixOS/home/common/programs/emacs/tabspaces-config.el")
(load-file "~/nix/nixOS/home/common/programs/emacs/eglot-config.el")
(load-file "~/nix/nixOS/home/common/programs/emacs/indent-config.el")
(load-file "~/nix/nixOS/home/common/programs/emacs/web-config.el")
(load-file "~/nix/nixOS/home/common/programs/emacs/org-roam-config.el")

;; Not working plugins that I want to make work
;;(load-file "~/nix/nixOS/home/common/programs/emacs/scala-cli-repl-config.el")
;;(load-file "~/nix/nixOS/home/common/programs/emacs/dape-config.el")

;; General Settings
(setq make-backup-files nil) ; Disable backup files
(setq auto-save-default nil) ; Disable auto save

;; magit-forge
(with-eval-after-load 'magit
  (require 'forge))

;; org-mode
(require 'org-modern)
(with-eval-after-load 'org (global-org-modern-mode))

;; Turn off bell
(setq ring-bell-function 'ignore)

;; line-numbers
(setq display-line-numbers-type 'default)
( global-display-line-numbers-mode t)

(yas-global-mode 1) ; Enable YASnippet

;; Function to kill all buffers except the current one
(defun kill-all-buffers-except-current ()
"Ask for confirmation before killing all buffers except the current one."
(interactive)
(if (yes-or-no-p "Really kill all buffers except the current one? ")
    (let ((current-buffer (current-buffer)))
        (mapc 'kill-buffer (delq current-buffer (buffer-list)))
        (delete-other-windows))))

;; Rg
(require 'rg)

;; Flycheck
(require 'flycheck)
(global-flycheck-mode) ; Enable flycheck

;; Set keybindings for moving between windows
(global-set-key (kbd "C-h") 'windmove-left)
(global-set-key (kbd "C-j") 'windmove-down)
(global-set-key (kbd "C-k") 'windmove-up)
(global-set-key (kbd "C-l") 'windmove-right)

;; Which Key
(require 'which-key)
(which-key-mode)

;; Ivy & Counsel
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

;; Enable SQL
(require 'sql)

(defun upcase-sql-keywords ()
(interactive)
(save-excursion
    (dolist (keywords sql-mode-postgres-font-lock-keywords)
    (goto-char (point-min))
    (while (re-search-forward (car keywords) nil t)
        (goto-char (+ 1 (match-beginning 0)))
        (when (eql font-lock-keyword-face (face-at-point))
        (backward-char)
        (upcase-word 1)
        (forward-char))))))

;; Copilot
;;(require 'copilot)
;;(add-hook 'prog-mode-hook 'copilot-mode)
;;(define-key copilot-completion-map (kbd "C-p") 'copilot-accept-completion)
;;(define-key copilot-mode-map (kbd "C-j") #'copilot-next-completion)
;;(define-key copilot-mode-map (kbd "C-k") #'copilot-previous-completion)
;;(add-to-list 'copilot-major-mode-alist '("scala-ts" . "scala"))

;; Xml Pretty Print
(defun xml-pretty-print (beg end &optional arg)
"Reformat the region between BEG and END.
With optional ARG, also auto-fill."
(interactive "*r\nP")
(shell-command-on-region beg end "xmllint --format -" t t))

;; Company
(require 'company)
(setq company-backends '(company-capf))
(add-hook 'after-init-hook 'global-company-mode)

;; Direnv Configuration
(direnv-mode)

;; EditorConfig
(require 'editorconfig)
(editorconfig-mode 1)

;; Font and Theme
(require-theme 'modus-themes)

(setq modus-themes-bold-constructs t
      modus-themes-italic-constructs t)
;; Maybe define some palette overrides, such as by using our presets
(setq modus-themes-common-palette-overrides
      modus-themes-preset-overrides-warmer)

(defvar my-current-theme 'modus-operandi-tinted)

(defun my-toggle-modus-theme ()
  (interactive)
  (if (display-graphic-p)
      (if (eq my-current-theme 'modus-operandi-tinted)
          (progn
            (load-theme 'modus-vivendi-tinted t)
            (setq my-current-theme 'modus-vivendi-tinted))
        (progn
          (load-theme 'modus-operandi-tinted t)
          (setq my-current-theme 'modus-operandi-tinted)))
    (message "Cannot toggle theme in non-graphical frame")))

(add-hook 'after-make-frame-functions
  (lambda (frame)
    (with-selected-frame frame
      ;; All customizations here
      (load-theme 'modus-operandi-tinted)
      (set-frame-font "Iosevka Nerd Font 12" nil t)
      (company-quickhelp-mode))))

;; Mood-line for a better mode-line
(require 'mood-line)
(mood-line-mode 1)
(setq mood-line-glyph-alist mood-line-glyphs-unicode)

;; Some hooks
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'tabspaces-mode)

(server-start)