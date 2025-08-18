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

;; Turn on which-key-mode
(add-hook 'after-init-hook 'which-key-mode)

;; Turn off autosave-mode
;; turn off backup-files
(auto-save-mode -1)
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Set global visual line mode for line-wrapping
(global-visual-line-mode t)

(minimal-emacs-load-user-init "custom.el")

(minimal-emacs-load-user-init "looks.el")

(minimal-emacs-load-user-init "org.el")

(minimal-emacs-load-user-init "org.el")

(minimal-emacs-load-user-init "tabspaces.el")

;;; Completions
(minimal-emacs-load-user-init "completions.el")

;;; Detached
(minimal-emacs-load-user-init "detached.el")

;;; Denote
(minimal-emacs-load-user-init "denote.el")

;;; LSP
(minimal-emacs-load-user-init "lsp.el")

;;; Dirvish
;; (minimal-emacs-load-user-init "dirvish.el") Use plain dired for now

;;; Modeline
(minimal-emacs-load-user-init "modeline.el")

;;; Tramp
(minimal-emacs-load-user-init "tramp.el")

;;; Custom nix stuff
(minimal-emacs-load-user-init "nix-config.el")

;;; Formatting
(minimal-emacs-load-user-init "apheleia.el")

;;; Iedit
(minimal-emacs-load-user-init "iedit.el")

;;; Meow
(minimal-emacs-load-user-init "meow.el")

;;; Direnv integration
(use-package envrc
  :hook (after-init . envrc-global-mode))

;;; Indentation
(use-package indent-bars
  :hook ((haskell-ts-mode scala-mode scala-ts-mode yaml-mode python-ts-mode) . indent-bars-mode))

;;; Lisp
;; (use-package sly
;;   :ensure t
;;   :config
;;   (setq sly-lisp-implementations
;;         '((sbcl ("sbcl") :coding-system utf-8-unix)))
;;   ;; Optional: Set the default implementation
;;   (setq sly-default-lisp 'sbcl))

;;; PDFs
(use-package pdf-tools
  :ensure t
  :hook (pdf-view-mode . (lambda ()
			               (display-line-numbers-mode -1)))
  :config
  (pdf-loader-install)
  (setq-default pdf-view-display-size 'fit-page))

;;; Terminal
(use-package eat)

(add-hook 'eshell-load-hook #'eat-eshell-mode)
(add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)

(global-set-key (kbd "M-o n")  'windmove-left)
(global-set-key (kbd "M-o o") 'windmove-right)
(global-set-key (kbd "M-o i")    'windmove-up)
(global-set-key (kbd "M-o e")  'windmove-down)
