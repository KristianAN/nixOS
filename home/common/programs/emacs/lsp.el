;;; lsp.el --- LSP configuration -*- no-byte-compile: t; lexical-binding: t; -*-

(defun list-active-faces ()
  "List all faces used in the current buffer."
  (interactive)
  (let ((faces '()))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((face (get-text-property (point) 'face)))
          (when face
            (unless (listp face) (setq face (list face)))
            (dolist (f face)
              (unless (memq f faces)
                (push f faces)))))
        (forward-char 1)))
    (message "Faces in use: %s" faces)
    faces))

(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-level 4)
  :config
  (seq-do (lambda (it)
            (push it major-mode-remap-alist))
          '((python-mode . python-ts-mode)
            (javascript-mode . js-ts-mode)
            (typescript-mode . typescript-ts-mode)
            (java-mode . java-ts-mode)
            (css-mode . css-ts-mode)
            (sh-mode . bash-ts-mode)
            (scala-mode . scala-ts-mode)
            (shell-script-mode . bash-ts-mode)))
  
  (defun my-custom-font-lock-settings ()
    "Set up JavaScript Tree-sitter mode with custom face settings."
    (setq-local face-remapping-alist
                (append face-remapping-alist
                        `((font-lock-string-face . (:foreground ,(car (ef-themes-with-colors (list yellow)))))
                          (font-lock-number-face . (:foreground ,(car (ef-themes-with-colors (list yellow)))))
                          (font-lock-comment-face . (:foreground ,(car (ef-themes-with-colors (list red)))))
                          (font-lock-function-name-face . default)
                          (font-lock-variable-name-face . default)
                          ))))

  ;; Add hooks
  (add-hook 'scala-ts-mode-hook 'my-custom-font-lock-settings)
  (add-hook 'java-ts-mode-hook 'my-custom-font-lock-settings)
  )

(use-package scala-ts-mode
  :ensure t
  :custom
  (scala-ts-indent-offset 2))

(use-package haskell-ts-mode
  :ensure t
  :custom
  (haskell-ts-use-indent t)
  (haskell-ts-ghci "ghci")
  :mode (("\\.hs\\'" . haskell-ts-mode)))

(use-package nix-ts-mode
  :mode "\\.nix\\'")

(use-package typescript-ts-mode
  :mode (("\\.cts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . typescript-ts-mode)
         ("\\.ts\\'" . typescript-ts-mode)))

(use-package python-ts-mode
  :mode ("\\.py\\'" . python-ts-mode))

(use-package fsharp-mode
  :defer t
  :ensure t)

(use-package web-mode
  :ensure t
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.php\\'" . web-mode)
   ("\\.vue\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  :custom
  (lsp-keymap-prefix "C-c l")
  ;; (lsp-completion-provider :none)       ; Use corfu instead for lsp completions
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)

(use-package lsp-haskell
  :ensure t)

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
