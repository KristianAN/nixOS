;;; post-init.el --- Post Init -*- no-byte-compile: t; lexical-binding: t; -*-

;; Ensure Emacs loads the most recent byte-compiled files.
(setq load-prefer-newer t)

;; Ensure JIT compilation is enabled for improved performance by
;; native-compiling loaded .elc files asynchronously
(setq native-comp-jit-compilation t)
;(setq native-comp-deferred-compilation t) ; Deprecated in Emacs > 29.1

;(use-package compile-angel
;  :ensure t
;  :demand t
;  :config
;  (compile-angel-on-load-mode)
;  (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode))

;; Auto-revert in Emacs is a feature that automatically updates the
;; contents of a buffer to reflect changes made to the underlying file
;; on disk.
(add-hook 'after-init-hook #'global-auto-revert-mode)

;; recentf is an Emacs package that maintains a list of recently
;; accessed files, making it easier to reopen files you have worked on
;; recently.
(add-hook 'after-init-hook #'recentf-mode)

;; savehist is an Emacs feature that preserves the minibuffer history between
;; sessions. It saves the history of inputs in the minibuffer, such as commands,
;; search strings, and other prompts, to a file. This allows users to retain
;; their minibuffer history across Emacs restarts.
(add-hook 'after-init-hook #'savehist-mode)

;; save-place-mode enables Emacs to remember the last location within a file
;; upon reopening. This feature is particularly beneficial for resuming work at
;; the precise point where you previously left off.
(add-hook 'after-init-hook #'save-place-mode)

;; Turn off autosave-mode
;; turn off backup-files
(auto-save-mode -1)
(setq make-backup-files nil)
(setq auto-save-default nil)

;;; Line numbers
(setq display-line-numbers-type 'relative) 
(global-display-line-numbers-mode)
;;; Font
;(set-frame-font "Iosevka Nerd Font" nil t)

(use-package nerd-icons)

(add-hook 'after-make-frame-functions
  (lambda (frame)
    (with-selected-frame frame
      ;; All customizations here
      (load-theme 'ef-symbiosis t)
      (set-frame-font "Iosevka Nerd Font 12" nil t)
      )))

;;; Terminal
(use-package vterm
  :ensure t
  :defer t
  :commands vterm
  :config
  ;; Speed up vterm
  (setq vterm-timer-delay 0.01))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

(minimal-emacs-load-user-init "custom.el")
(minimal-emacs-load-user-init "evil.el")

;;; Completions

(minimal-emacs-load-user-init "completions.el")

;;; LSP
(minimal-emacs-load-user-init "lsp.el")

;;; Modeline
(minimal-emacs-load-user-init "modeline.el")

;;; Direnv integration
(use-package envrc
  :hook (after-init . envrc-global-mode))

;;; Magit
(use-package magit)

;;; Projectile
(use-package projectile
  (setq projectile-project-search-path '("~/projects/" "~/src")))

;;; Indentation
(use-package indent-bars
  :hook ((scala-mode scala-ts-mode yaml-mode) . indent-bars-mode))

