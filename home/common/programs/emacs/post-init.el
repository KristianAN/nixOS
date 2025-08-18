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

(minimal-emacs-load-user-init "completions.el")

(minimal-emacs-load-user-init "looks.el")

(minimal-emacs-load-user-init "org.el")

(minimal-emacs-load-user-init "org.el")

(minimal-emacs-load-user-init "tabspaces.el")

;;; Detached
(minimal-emacs-load-user-init "detached.el")

;;; Denote
(minimal-emacs-load-user-init "denote.el")

;;; LSP
(minimal-emacs-load-user-init "lsp.el")

;;; Dirvish
;; (minimal-emacs-load-user-init "dirvish.el") Use plain dired for now

;;; Tramp
(minimal-emacs-load-user-init "tramp.el")

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

;;; Terminal
(use-package eat)

;;; delight
(use-package delight
  :ensure t
  :config
  (delight '((envrc-mode nil "envrc")               ; Hide envrc-mode
             (which-key-mode nil "which-key")       ; Hide which-key
             (apheleia-mode nil "apheleia")         ; Hide apheleia
             (visual-line-mode nil "simple")        ; Hide wrap mode
             
             ;; Other unwanted minor modes
             (auto-revert-mode nil "autorevert")
             (eldoc-mode nil "eldoc")
             (company-mode nil "company")
             (yas-minor-mode nil "yasnippet")))
  
  ;; Force hide vc-mode specifically
  (with-eval-after-load 'vc-hooks
    (setq-default mode-line-format
                  (delete '(vc-mode vc-mode) mode-line-format))))

(add-hook 'eshell-load-hook #'eat-eshell-mode)
(add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)

(global-set-key (kbd "M-o n")  'windmove-left)
(global-set-key (kbd "M-o o") 'windmove-right)
(global-set-key (kbd "M-o i")    'windmove-up)
(global-set-key (kbd "M-o e")  'windmove-down)
