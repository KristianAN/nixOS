;;; evil-config --- configuration for evil-mode
;;; Commentary:
;;; Code:

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init '(calendar dired magit org org-roam)))

;; Set the leader key to space
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "b" 'project-switch-to-buffer
  "f" 'consult-find
  "k" 'kill-buffer
  "q" 'kill-all-buffers-except-current
  "w" 'save-buffer
  "F" 'consult-ripgrep
  "d" 'dired
  "t" 'tabspaces-command-map
  "/" 'magit-status
  "<SPC>" 'project-find-file

  ;; Lsp
  "gd" 'lsp-find-definition
  "gr" 'lsp-find-references
  "gn" 'lsp-rename
  "gf" 'lsp-format-buffer
  "ga" 'lsp-execute-code-action
  "gh" 'lsp-inlay-hints-mode

  ;; Project
  "ps" 'project-eshell
  "pb" 'project-switch-to-buffer
  "pt" 'project-vterm

  ;; Modus theme toggle
  "mt" 'my-toggle-modus-theme
  
  ;; Org Keybindings
  "oa" 'org-agenda
  "oc" 'org-capture

  ;; Org-roam keybindings
  "orf" 'org-roam-node-find
  "ori" 'org-roam-node-insert
  "ogc" 'org-roam-capture

  ;; rg.el
  "sp" 'rg-project
  "sdp" 'rg-dwim-project-dir
  "sd" 'rg-dwim

  ;; vterm
  "vp" 'multi-vterm-project
  "vn" 'multi-vterm-next
  "vb" 'multi-vterm-previous
  "vt" 'multi-vterm-dedicated-toggle
  "vm" 'multi-vterm
  )

;; For scala-ts-mode
(with-eval-after-load 'scala-ts-mode
  (evil-define-key 'normal scala-ts-mode-map
    "go" 'lsp-organize-imports
    "gc" 'lsp-metals-new-scala-file
    ))

;; For java-ts-mode
(with-eval-after-load 'java-ts-mode
  (evil-define-key 'normal java-ts-mode-map
    "go" 'lsp-java-organize-imports
    )) 
;;; evil-config.el ends here

