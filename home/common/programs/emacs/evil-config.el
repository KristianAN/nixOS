;;; evil-config --- configuration for evil-mode
;;; Commentary:
;;; Code:
(setq evil-want-keybinding nil)
(require 'evil)
(evil-mode 1)
(evil-collection-init '(calendar dired magit org org-roam))

;; Set the leader key to space
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "b" 'project-switch-to-buffer
  "f" 'counsel-find-file
  "k" 'kill-buffer
  "q" 'kill-all-buffers-except-current
  "w" 'save-buffer
  "x" 'counsel-M-x
  "F" 'counsel-rg
  "d" 'dired
  "t" 'tabspaces-command-map
  "/" 'magit-status
  "<SPC>" 'project-find-file

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

  ;; Lsp keybinds
  "gd" 'xref-find-definitions
  "gr" 'xref-find-references
  "gn" 'eglot-rename
  "gf" 'eglot-format
  "ga" 'eglot-code-actions
  "gt" 'eglot-find-typeDefinition
  "gh" 'eglot-inlay-hints-mode

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
;;; evil-config.el ends here

