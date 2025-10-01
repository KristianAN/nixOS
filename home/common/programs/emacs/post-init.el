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

(emacs-load-user-configuration "custom.el")

(emacs-load-user-configuration "completions.el")

(emacs-load-user-configuration "looks.el")

(emacs-load-user-configuration "org.el")

(emacs-load-user-configuration "org.el")

;;; Detached
(emacs-load-user-configuration "detached.el")

;;; Denote
(emacs-load-user-configuration "denote.el")

;;; LSP
(emacs-load-user-configuration "lsp.el")

;;; Dirvish
;; (emacs-load-user-configuration "dirvish.el") Use plain dired for now

;;; Tramp
(emacs-load-user-configuration "tramp.el")

;;; Formatting
(emacs-load-user-configuration "apheleia.el")

;;; Iedit
(emacs-load-user-configuration "iedit.el")

;;; Flyspell
(emacs-load-user-configuration "flyspell.el")

;;; Presentation
(emacs-load-user-configuration "present.el")

;;; Eat 
(emacs-load-user-configuration "eat.el")

;;; Avy
(emacs-load-user-configuration "avy.el")

;;; Meow
;;; (emacs-load-user-configuration "meow.el")

;;; Direnv integration
(use-package envrc
  :hook (after-init . envrc-global-mode))

;;; Indentation
(use-package indent-bars
  :hook ((haskell-ts-mode scala-mode scala-ts-mode yaml-mode python-ts-mode) . indent-bars-mode))


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

(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter)) 

(add-hook 'eshell-load-hook #'eat-eshell-mode)
(add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)

