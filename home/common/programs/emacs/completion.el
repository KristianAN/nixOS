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
  (
   consult-ripgrep
   consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref consult--source-recent-file consult--source-project-recent-file consult--source-bookmark)

  (global-set-key [remap switch-to-buffer] 'consult-buffer)
  (global-set-key [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
  (global-set-key [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame)
  (global-set-key [remap goto-line] 'consult-goto-line)

  )

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)
  )

;; Corfu
(use-package corfu
  ;; TAB-and-Go customizations
  :custom
    (corfu-cycle t)           ;; Enable cycling for `corfu-next/previous'
    (corfu-preselect 'prompt) ;; Always preselect the prompt
      ;; Use TAB for cycling, default is `corfu-complete'.
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :config
  (setq corfu-auto t
    corfu-quit-no-match 'separator) 
  :init
  (corfu-popupinfo-mode)
  (global-corfu-mode))

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

;;; completion.el ends here
