;;; post-init.el --- Post Init -*- no-byte-compile: t; lexical-binding: t; -*-

;; Ensure Emacs loads the most recent byte-compiled files.
(setq load-prefer-newer t)

;; Ensure JIT compilation is enabled for improved performance by
;; native-compiling loaded .elc files asynchronously
(setq native-comp-jit-compilation t)
                                        ;(setq native-comp-deferred-compilation t) ;

;; Auto-revert in Emacs is a feature that automatically updates the
;; contents of a buffer to reflect changes made to the underlying file
;; on disk.
(use-package autorevert
  :ensure nil
  :commands (auto-revert-mode global-auto-revert-mode)
  :hook
  (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-interval 3)
  (auto-revert-remote-files nil)
  (auto-revert-use-notify t)
  (auto-revert-avoid-polling nil)
  (auto-revert-verbose t))

;; Recentf is an Emacs package that maintains a list of recently
;; accessed files, making it easier to reopen files you have worked on
;; recently.
(use-package recentf
  :ensure nil
  :commands (recentf-mode recentf-cleanup)
  :hook
  (after-init . recentf-mode)

  :custom
  (recentf-auto-cleanup (if (daemonp) 300 'never))
  (recentf-exclude
   (list "\\.tar$" "\\.tbz2$" "\\.tbz$" "\\.tgz$" "\\.bz2$"
         "\\.bz$" "\\.gz$" "\\.gzip$" "\\.xz$" "\\.zip$"
         "\\.7z$" "\\.rar$"
         "COMMIT_EDITMSG\\'"
         "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
         "-autoloads\\.el$" "autoload\\.el$"))

  :config
  ;; A cleanup depth of -90 ensures that `recentf-cleanup' runs before
  ;; `recentf-save-list', allowing stale entries to be removed before the list
  ;; is saved by `recentf-save-list', which is automatically added to
  ;; `kill-emacs-hook' by `recentf-mode'.
  (add-hook 'kill-emacs-hook #'recentf-cleanup -90))

;; savehist is an Emacs feature that preserves the minibuffer history between
;; sessions. It saves the history of inputs in the minibuffer, such as commands,
;; search strings, and other prompts, to a file. This allows users to retain
;; their minibuffer history across Emacs restarts.
(use-package savehist
  :ensure nil
  :commands (savehist-mode savehist-save)
  :hook
  (after-init . savehist-mode)
  :custom
  (savehist-autosave-interval 600)
  (savehist-additional-variables
   '(kill-ring                        ; clipboard
     register-alist                   ; macros
     mark-ring global-mark-ring       ; marks
     search-ring regexp-search-ring)))

;; save-place-mode enables Emacs to remember the last location within a file
;; upon reopening. This feature is particularly beneficial for resuming work at
;; the precise point where you previously left off.
(use-package saveplace
  :ensure nil
  :commands (save-place-mode save-place-local-mode)
  :hook
  (after-init . save-place-mode)
  :custom
  (save-place-limit 400))

(use-package which-key
  :ensure nil ; builtin
  :commands which-key-mode
  :hook (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 1.5)
  (which-key-idle-secondary-delay 0.25)
  (which-key-add-column-padding 1)
  (which-key-max-description-length 40))

(emacs-load-user-configuration "completions.el")

(emacs-load-user-configuration "looks.el")

(emacs-load-user-configuration "org.el")

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

;;; Avy
(emacs-load-user-configuration "avy.el")

;;; Custom functions and configuration
(emacs-load-user-configuration "custom.el")

;;; Eshell configuration
(emacs-load-user-configuration "eshell.el")

;;; Meow configuration
(emacs-load-user-configuration "meow.el")

;;; Direnv integration
(use-package envrc
  :defer t
  :hook (after-init . envrc-global-mode))

;;; Indentation
(use-package indent-bars
  :defer t
  :hook ((haskell-ts-mode scala-mode scala-ts-mode yaml-mode python-ts-mode) . indent-bars-mode))


(use-package consult-hoogle
  :ensure t 
  ;;  :load-path "~/projects/consult-hoogle"
  )

(use-package magit
  :ensure t
  :defer t
  :bind
  (("C-c g g" . magit)
   ("C-c g b" . magit-blame)))

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

  )

(use-package ansi-color
  :hook (
         (compilation-filter . ansi-color-compilation-filter)))


