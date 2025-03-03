;;; evil.el --- EVIL configuration -*- no-byte-compile: t; lexical-binding: t; -*-

;; evil-want-keybinding must be declared before Evil and Evil Collection
(setq evil-want-keybinding nil)

(use-package evil
  :ensure t
  :init
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :custom
  (evil-want-Y-yank-to-eol t)
  :config
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-mode 1))

;;; Leader
(use-package evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "f" '("find" . (keymap))
  "fb" '("buffer" . consult-buffer)
  "fg" '("grep" . consult-ripgrep)
  "fl" '("line" . consult-line)
  "fw" '("grep-selection" . kristian/consult-ripgrep-from-visual-selection)
  "G" 'magit
  "p" '("project" . (keymap))
  "pp" '("switch-project" . project-switch-project)
  "pt" '("term" . eat-project)
  "po" '("term-other-window" . eat-project-other-window)
  "<SPC>" '("find-file" . project-find-file)
  "m" '("files" . (keymap))
  "mc" 'dired-jump
  "l" '("lsp" . (keymap))
  "ld" 'flymake-show-project-diagnostics
  "la" 'eglot-code-actions
  "ln" 'flymake-goto-next-error
  "le" 'eglot
  "e"  'eldoc-box-help-at-point
  )

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package undo-fu
  :ensure t
  :commands (undo-fu-only-undo
             undo-fu-only-redo
             undo-fu-only-redo-all
             undo-fu-disable-checkpoint)
  :custom
  ;; 3 times the default values
  (undo-limit (* 3 160000))
  (undo-strong-limit (* 3 240000)))

(use-package undo-fu-session
  :ensure t
  :config
  (undo-fu-session-global-mode))

(use-package vim-tab-bar
  :ensure t
  :commands vim-tab-bar-mode
  :hook (after-init . vim-tab-bar-mode))

(use-package evil-visualstar
  :after evil
  :ensure t
  :defer t
  :commands global-evil-visualstar-mode
  :hook (after-init . global-evil-visualstar-mode))

(use-package evil-surround
  :after evil
  :ensure t
  :defer t
  :commands global-evil-surround-mode
  :custom
  (evil-surround-pairs-alist
   '((?\( . ("(" . ")"))
     (?\[ . ("[" . "]"))
     (?\{ . ("{" . "}"))

     (?\) . ("(" . ")"))
     (?\] . ("[" . "]"))
     (?\} . ("{" . "}"))

     (?< . ("<" . ">"))
     (?> . ("<" . ">"))))
  :hook (after-init . global-evil-surround-mode))

(with-eval-after-load "evil"
  (evil-define-operator my-evil-comment-or-uncomment (beg end)
    "Toggle comment for the region between BEG and END."
    (interactive "<r>")
    (comment-or-uncomment-region beg end))
  (evil-define-key 'normal 'global (kbd "gc") 'my-evil-comment-or-uncomment))


