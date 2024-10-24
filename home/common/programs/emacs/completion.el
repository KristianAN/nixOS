;;; completion --- configuration of various completion frameworks
;;; Commentary:
;;; Code:

;; Orderless
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))


;; Marginalia
(use-package marginalia
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; Consult
(use-package consult 
  :ensure t 
  :bind (
	 ("C-c s l" . consult-line)
	 ("C-c s b" . consult-buffer)
	 ("C-c s B" . consult-project-buffer) 
	 ("C-c s i" . consult-imenu)
	 ("C-c s o" . consult-outline)
	 ("C-c s g" . consult-ripgrep)
	 ("C-c s f" . consult-find)
	 ("C-c s r" . consult-recent-file)
	 ("C-c s G" . consult-git-grep))
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  )

;; Enable vertico
(use-package vertico
  :bind(:map vertico-map 
    ("C-n" . vertico-next)
    ("C-p" . vertico-previous)
    ("C-y" . vertico-insert))
  :init
  (vertico-mode)
  )

;; company
(use-package company
  :defer t 
  :ensure t
  :custom
  (company-tooltip-align-annotations t)      ;; Align annotations with completions.
  (company-minimum-prefix-length 1)          ;; Trigger completion after typing 1 character
  (company-idle-delay 0.2)                   ;; Delay before showing completion (adjust as needed)
  (company-tooltip-maximum-width 50) 
  :config

  ;; While using C-p C-n to select a completion candidate
  ;; C-y quickly shows help docs for the current candidate
  (define-key company-active-map (kbd "C-h")
			  (lambda ()
				(interactive)
				(company-show-doc-buffer)))
  (define-key company-active-map (kbd "C-y") 'company-complete-selection)
  :hook
  (after-init . global-company-mode)) ;; Enable Company Mode globally after initialization.

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))



;; A few more useful configurations...
(use-package emacs
  :init
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  (setq tab-always-indent 'complete)
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (setq enable-recursive-minibuffers t))


(defun kb/corfu-setup-lsp ()
    "Use orderless completion style with lsp-capf instead of the
default lsp-passthrough."
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
;;; completion.el ends here
