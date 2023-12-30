{ pkgs, ... }:
let
  # Emacs Copilot Installation Definition
  emacsCopilotSrc = builtins.fetchGit {
    url = "https://github.com/zerolfx/copilot.el.git";
    rev = "421703f5dd5218ec2a3aa23ddf09d5f13e5014c2";
  };
  scalaTsModeSrc = builtins.fetchGit {
    url = "https://github.com/KaranAhlawat/scala-ts-mode.git";
    rev = "cbfab189842ce564d9514f1b65a72b0af0d51438";
  };

  kanagawaThemeSrc = builtins.fetchGit {
    url = "https://github.com/meritamen/emacs-kanagawa-theme.git";
    rev = "cd4869986e0a3f688131007f1366f6041ef8d818";
  };
in
{
  programs.emacs = {
    enable = true;

    extraPackages = epkgs:
      with epkgs; [
        # Core packages
        evil # Extensible Vi Layer for Emacs
        evil-collection # A set of keybindings for evil mode
        evil-nerd-commenter # Comment/uncomment lines efficiently
        evil-surround # Emulates vim-surround functionality in Emacs
        evil-leader # A minor mode for Emacs that emulates Vim's "leader" key
        general # Provides a more convenient way to define keybindings
        which-key # Displays available keybindings in popup

        # Optional packages.
        all-the-icons # A package for inserting developer icons
        all-the-icons-dired # Shows icons for each file in dired mode
        all-the-icons-ivy-rich # More friendly display transformer for ivy
        dired-single # Reuse the dired buffer
        direnv # Environment switcher for Emacs
        editorconfig # EditorConfig Emacs Plugin
        eldoc # Show function arglist or variable docstring in echo area
        emojify # Display emojis in Emacs
        eshell-prompt-extras # Display extra information and color for your eshell prompt
        flycheck # On-the-fly syntax checking
        hydra # Make bindings that stick around
        ivy # A generic completion mechanism
        ivy-posframe # Display ivy in a posframe
        ivy-prescient # Better sorting and filtering for ivy
        ivy-rich # More friendly display transformer for ivy
        ligature # Ligature support for Emacs
        magit # A Git porcelain inside Emacs
        nerd-icons # Nerd icons for Emacs
        nix-mode # Nix integration
        org # For keeping notes, maintaining TODO lists, and project planning
        org-drill # A spaced repetition system for Emacs
        org-pomodoro # Pomodoro technique implementation
        org-roam # A note-taking tool based on the principles of networked thought
        org-roam-ui # A graphical user interface for org-roam
        pretty-mode # Redisplay parts of the buffer as pretty symbols
        projectile # Project Interaction Library for Emacs

        # Terminal
        vterm # Fully-featured terminal emulator
        multi-vterm # Manage multiple vterm buffers

        # Theme
        doom-modeline # A fancy and fast mode-line

        # Language Server
        dap-mode # Debug Adapter Protocol mode

        # Programming language packages.
        company # Modular text completion framework
        helm-xref # Helm UI for xref
        json-mode # Major mode for editing JSON files
        vue-mode # Major mode for editing Vue.js files
        yaml-mode # Major mode for editing YAML files
        yasnippet # Template system for Emacs
        clojure-mode # Major mode for editing clojure files
        cider # Extends clojure-mode with superpowers
        web-mode # Major mode for editing web templates

        # User interface packages.
        counsel # Various completion functions using Ivy
      ];
    extraConfig = ''
      (setq inhibit-startup-message t) ; Disable startup message
      (menu-bar-mode -1) ; Disable the menu bar
      (tool-bar-mode -1) ; Disable the toolbar
      (scroll-bar-mode -1) ; Disable the scroll bar
      (setq-default indent-tabs-mode nil) ; Use spaces instead of tabs

      (setq display-line-numbers-type 'default)
      (global-display-line-numbers-mode t)

      ;; Package Specific Settings

      (yas-global-mode 1) ; Enable YASnippet

      ;; Function to kill all buffers except the current one
      (defun kill-all-buffers-except-current ()
        "Ask for confirmation before killing all buffers except the current one."
        (interactive)
        (if (yes-or-no-p "Really kill all buffers except the current one? ")
            (let ((current-buffer (current-buffer)))
              (mapc 'kill-buffer (delq current-buffer (buffer-list)))
              (delete-other-windows))))

      ;; Evil Mode
      (require 'evil)
      (evil-mode 1)

      ;; Set the leader key to space
      (global-evil-leader-mode)
      (evil-leader/set-leader "<SPC>")
      (evil-leader/set-key
        "b" 'ivy-switch-buffer
        "f" 'counsel-find-file
        "k" 'kill-buffer
        "q" 'kill-all-buffers-except-current
        "w" 'save-buffer
        "x" 'counsel-M-x
        "F" 'counsel-git-grep
        "d" 'dired
        "t" 'vterm
        "p" 'projectile-command-map
        "/" 'magit-status

        ;; Org Keybindings
        "oa" 'org-agenda
        "oc" 'org-capture
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

      ;; Projectile
      (setq projectile-project-search-path '("~/src/" ))
      (defvar projectile-project-root nil)
      (projectile-mode +1)

      ;; Enable Scala
      ;; scala-ts-mode configuration
      (let ((scala-ts-mode-dir "~/.scalaTsMode")
            (scala-ts-mode-file "~/.scalaTsMode/scala-ts-mode.el"))
        ;; Check if the scala-ts-mode.el file exists
        (when (file-exists-p scala-ts-mode-file)
          ;; Add the directory to the load-path
          (add-to-list 'load-path scala-ts-mode-dir)
          ;; Try to load the scala-ts-mode module and catch any errors
          (condition-case err
              (progn
                (require 'scala-ts-mode))
            ;; If there's an error, print a message (you can also log or take other actions)
            (error (message "Failed to load scala-ts-mode: %s" err)))))


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

      ;; Copilot Configuration
      (let ((copilot-dir "~/.emacsCopilot")
            (copilot-file "~/.emacsCopilot/copilot.el"))
        ;; Check if the copilot.el file exists
        (when (file-exists-p copilot-file)
          ;; Add the directory to the load-path
          (add-to-list 'load-path copilot-dir)
          ;; Try to load the copilot module and catch any errors
          (condition-case err
              (progn
                (require 'copilot)
                (add-hook 'prog-mode-hook 'copilot-mode)
                (define-key copilot-completion-map (kbd "C-p") 'copilot-accept-completion)
                (define-key copilot-mode-map (kbd "C-j") #'copilot-next-completion)
                (define-key copilot-mode-map (kbd "C-k") #'copilot-previous-completion))
            ;; If there's an error, print a message (you can also log or take other actions)
            (error (message "Failed to load copilot: %s" err)))))

      ;; Xml Pretty Print
      (defun xml-pretty-print (beg end &optional arg)
        "Reformat the region between BEG and END.
        With optional ARG, also auto-fill."
        (interactive "*r\nP")
        (shell-command-on-region beg end "xmllint --format -" t t))

      ;; Enable Tree-sitter langs
      (setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
       (scala "https://github.com/tree-sitter/tree-sitter-scala")
      ))

      
      (dolist (mode
           '(
             clojure-mode-hook
             clojurescript-mode-hook
             clojurec-mode-hook
             scala-ts-mode-hook
             ))
      (add-hook mode 'eglot-ensure))

      (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs '((scala-mode scala-ts-mode) . ("metals"))))

      (require 'company)
      (setq company-backends '(company-capf))
      (add-hook 'after-init-hook 'global-company-mode)

      ;; Direnv Configuration
      (direnv-mode)

      ;; EditorConfig
      (require 'editorconfig)
      (editorconfig-mode 1)

      ;; Vterm Configuration
      (require 'vterm)

      ;; Kanagawa theme
      (let ((kanagawa-theme-dir "~/.emacsKanagawaTheme")
            (kanagawa-theme-file "~/.emacsKanagawaTheme/kanagawa-theme.el"))
        ;; Check if the kanagawa-theme.el file exists
        (when (file-exists-p kanagawa-theme-file)
          ;; Add the directory to the load-path
          (add-to-list 'load-path kanagawa-theme-dir)
          ;; Try to load the kanagawa-theme module and catch any errors
          (condition-case err
              (progn
                (require 'kanagawa-theme))
            ;; If there's an error, print a message (you can also log or take other actions)
            (error (message "Failed to load kanagawa-theme: %s" err)))))

      (load-theme 'kanagawa t)

    '';
  };

  home.packages = with pkgs; [
    rnix-lsp
    metals
    clojure-lsp
    jdt-language-server
  ];

  home.file.".emacsCopilot".source = emacsCopilotSrc;
  home.file.".scalaTsMode".source = scalaTsModeSrc;
  home.file.".emacsKanagawaTheme".source = kanagawaThemeSrc;
}
