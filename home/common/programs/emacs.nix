{inputs, pkgs, ... }:
{
  programs.emacs = {
    enable = true;
    overrides = self: super: {
      # I install the packages below by hand because they're not in MELPA, and I
      # don't want to incur the startup cost of using straight.el.
      copilot =
        let
          rev = inputs.copilot-el.shortRev;
        in
        with pkgs;
        with pkgs.emacsPackages;
        melpaBuild {
          pname = "copilot";
          ename = "copilot";
          version = inputs.copilot-el.lastModifiedDate;
          commit = rev;
          packageRequires = [ dash editorconfig s ];

          src = fetchFromGitHub {
            inherit rev;
            owner = "zerolfx";
            repo = "copilot.el";
            sha256 = inputs.copilot-el.narHash;
          };

          recipe = writeText "recipe" ''
            (copilot
              :repo "zerolfx/copilot.el"
              :fetcher github
              :files ("*.el" "dist"))
          '';
          meta.description = "Emacs plugin for GitHub Copilot";
        };
        scala-ts-mode =
        let
          rev = inputs.scala-ts-mode.shortRev;
        in
        with pkgs;
        with pkgs.emacsPackages;
        melpaBuild {
          pname = "scala-ts-mode";
          ename = "scala-ts-mode";
          version = inputs.scala-ts-mode.lastModifiedDate;
          commit = rev;
          packageRequires = [ ];

          src = fetchFromGitHub {
            inherit rev;
            owner = "KaranAhlawat";
            repo = "scala-ts-mode";
            sha256 = inputs.scala-ts-mode.narHash;
          };

          recipe = writeText "recipe" ''
            (scala-ts-mode
              :repo "KaranAhlawat/scala-ts-mode"
              :fetcher github
              :files ("*.el" "dist"))
          '';
          meta.description = "Emacs plugin for Scala tree-sitter support";
        };

    };

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
        doom-themes # An opinionated pack of modern color-themes
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
        scala-ts-mode # Major mode for editing Scala files
        eglot-java# Majos mode for editing Java files
        # copilot # Copilot support

        # User interface packages.
        counsel # Various completion functions using Ivy
        treesit-grammars.with-all-grammars
      ];

    extraConfig = builtins.readFile ./init.el;
      
  };

  home.packages = with pkgs; [
    rnix-lsp
  ];

  services.emacs.enable = true;

}
