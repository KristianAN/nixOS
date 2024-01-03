;;; init --- Extra configuration for emacs in nix
;;; Commentary:

;;; Code:
(setq inhibit-startup-message t) ; Disable startup message
(menu-bar-mode -1) ; Disable the menu bar
(tool-bar-mode -1) ; Disable the toolbar
(scroll-bar-mode -1) ; Disable the scroll bar
(setq-default indent-tabs-mode nil) ; Use spaces instead of tabs

(setq display-line-numbers-type 'default)
(global-display-line-numbers-mode t)

;; Package Specific Settings

;; vterm
(require 'vterm)
(defun project-vterm ()
  (interactive)
  (let* ((default-directory (project-root (project-current t)))
         (vterm-buffer-name "*vterm*")
         (vterm-buffer (get-buffer vterm-buffer-name)))
    (if (and vterm-buffer (not current-prefix-arg))
        (pop-to-buffer vterm-buffer)
      (vterm))))

;; git-gutter
(require 'git-gutter)
(add-hook 'prog-mode-hook 'git-gutter-mode)
(setq git-gutter:update-interval 0.05)

;; git-gutter-fringe
(defconst fringe-size '8 "Default fringe width.")

;;; Setting up the fringe
;; switches order of fringe and margin
;;(setq-default fringes-outside-margins t)

;; standardize fringe width
(fringe-mode fringe-size)
(push `(left-fringe  . ,fringe-size) default-frame-alist)
(push `(right-fringe . ,fringe-size) default-frame-alist)

;; colored fringe "bars"
(define-fringe-bitmap 'git-gutter-fr:added
	  [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
	  nil nil 'center)
(define-fringe-bitmap 'git-gutter-fr:modified
	  [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
	  nil nil 'center)
(define-fringe-bitmap 'git-gutter-fr:deleted
	  [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
	  nil nil 'center)

;; Tabspaces
(require 'tabspaces)
(setq
    tabspaces-use-filtered-buffers-as-default t
    tabspaces-default-tab "Default"
    tabspaces-remove-to-default t
    tabspaces-include-buffers '("*scratch*")
    tabspaces-initialize-project-with-todo t
    tabspaces-todo-file-name "project-todo.org"
)

;;Tabspaces sessions
(setq tabspaces-session t)
(setq tabspaces-session-auto-restore t)

(yas-global-mode 1) ; Enable YASnippet

;; Function to kill all buffers except the current one
(defun kill-all-buffers-except-current ()
"Ask for confirmation before killing all buffers except the current one."
(interactive)
(if (yes-or-no-p "Really kill all buffers except the current one? ")
    (let ((current-buffer (current-buffer)))
        (mapc 'kill-buffer (delq current-buffer (buffer-list)))
        (delete-other-windows))))

;; Function to toggle theme
(defun my-toggle-modus-theme ()
  (interactive)
  (if (display-graphic-p)
      (modus-themes-toggle)
    (message "Cannot toggle theme in non-graphical frame")))

;; Rg
(require 'rg)

;; Evil Mode
(setq evil-want-keybinding nil)
(require 'evil)
(evil-mode 1)
(evil-collection-init '(calendar dired magit org org-roam))

;; Set the leader key to space
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "b" 'counsel-switch-buffer
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
  "ps" 'project-eshell
  "pb" 'project-switch-to-buffer
  "pt" 'project-vterm
  ;; Modus theme toggle
  "mt" 'my-toggle-modus-theme
  
  ;; Org Keybindings
  "oa" 'org-agenda
  "oc" 'org-capture

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
)

;; Flycheck
(require 'flycheck)
(global-flycheck-mode) ; Enable flycheck

;; Set keybindings for moving between windows
(global-set-key (kbd "C-h") 'windmove-left)
(global-set-key (kbd "C-j") 'windmove-down)
(global-set-key (kbd "C-k") 'windmove-up)
(global-set-key (kbd "C-l") 'windmove-right)

;; Which Key
(require 'which-key)
(which-key-mode)

;; Ivy & Counsel
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

;; Enable SQL
(require 'sql)

(defun upcase-sql-keywords ()
(interactive)
(save-excursion
    (dolist (keywords sql-mode-postgres-font-lock-keywords)
    (goto-char (point-min))
    (while (re-search-forward (car keywords) nil t)
        (goto-char (+ 1 (match-beginning 0)))
        (when (eql font-lock-keyword-face (face-at-point))
        (backward-char)
        (upcase-word 1)
        (forward-char))))))

;; Javascript Environment
(helm-mode +1)
(require 'web-mode)
(require 'helm-xref)
(require 'vue-mode)
(require 'json-mode)
(require 'dap-chrome)
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; Web-mode configurations
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.liquid\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(setq web-mode-enable-auto-closing t)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-part-padding 2)
(setq web-mode-script-padding 2)
(setq web-mode-style-padding 2)
(setq web-mode-code-indent-offset 2)

;; Copilot
;;(require 'copilot)
;;(add-hook 'prog-mode-hook 'copilot-mode)
;;(define-key copilot-completion-map (kbd "C-p") 'copilot-accept-completion)
;;(define-key copilot-mode-map (kbd "C-j") #'copilot-next-completion)
;;(define-key copilot-mode-map (kbd "C-k") #'copilot-previous-completion)
;;(add-to-list 'copilot-major-mode-alist '("scala-ts" . "scala"))

;; Xml Pretty Print
(defun xml-pretty-print (beg end &optional arg)
"Reformat the region between BEG and END.
With optional ARG, also auto-fill."
(interactive "*r\nP")
(shell-command-on-region beg end "xmllint --format -" t t))

(setq major-mode-remap-alist
'((yaml-mode . yaml-ts-mode)
    (bash-mode . bash-ts-mode)
    (js2-mode . js-ts-mode)
    (typescript-mode . typescript-ts-mode)
    (json-mode . json-ts-mode)
    (css-mode . css-ts-mode)
    (python-mode . python-ts-mode)))

;; Tree-sitter-config
(setq treesit-font-lock-level 4)

;; Eglot
(dolist (mode
    '(
        clojure-mode-hook
        clojurescript-mode-hook
        clojurec-mode-hook
        scala-ts-mode-hook
        ))
(add-hook mode 'eglot-ensure))
(add-hook 'java-mode-hook 'eglot-java-mode)
(add-hook 'after-save-hook
          (lambda ()
            (when (eglot-managed-p)
              (eglot-format))))

(with-eval-after-load 'eglot
(add-to-list 'eglot-server-programs '((scala-mode scala-ts-mode) . ("metals"))))

;; Company
(require 'company)
(setq company-backends '(company-capf))
(add-hook 'after-init-hook 'global-company-mode)

;; Direnv Configuration
(direnv-mode)

;; EditorConfig
(require 'editorconfig)
(editorconfig-mode 1)

;; Font and Theme
(require-theme 'modus-themes)

(setq modus-themes-bold-constructs t
      modus-themes-italic-constructs t)
;; Maybe define some palette overrides, such as by using our presets
(setq modus-themes-common-palette-overrides
      modus-themes-preset-overrides-warmer)


(add-hook 'after-make-frame-functions
  (lambda (frame)
    (with-selected-frame frame
      ;; All customizations here
      (load-theme 'modus-operandi)
      (set-frame-font "Iosevka Nerd Font 12" nil t)
      (company-quickhelp-mode))))

;; Mood-line for a better mode-line
(require 'mood-line)
(mood-line-mode 1)
(setq mood-line-glyph-alist mood-line-glyphs-fira-code)

;; Some hooks
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'tabspaces-mode)
(dolist (indent-mode-langs
         '(
           scala-ts-mode-hook
           java-mode-hook
           clojure-mode-hook
           ))
(add-hook indent-mode-langs 'indent-bars-mode))

;; Turn off bell
(setq ring-bell-function 'ignore)

;;; Indent-guides
(require 'indent-bars)
(setq
    indent-bars-color '(highlight :face-bg t :blend 0.3)
    indent-bars-pattern " . . . . ." ; play with the number of dots for your usual font size
    indent-bars-width-frac 0.35
    indent-bars-pad-frac 0.2
    indent-bars-highlight-current-depth nil)

(server-start)
