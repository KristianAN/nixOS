{ inputs, pkgs, ... }:

{
  programs.emacs = {
    enable = true;
    overrides = self: super: {
      # I install the packages below by hand because they're not in MELPA, and I
      # don't want to incur the startup cost of using straight.el.

      dape =
        let
          rev = inputs.dape.shortRev;
        in
          with pkgs;
          with pkgs.emacsPackages;
          melpaBuild {
            pname = "dape";
            ename = "dape";
            version = inputs.dape.lastModifiedDate;
            commit = rev;
            packageRequires = [];

            src = fetchFromGitHub {
              inherit rev;
              owner = "svaante";
              repo = "dape";
              sha256 = inputs.dape.narHash;
            };

            recipe = writeText "recipe" ''
              (dape
                :repo "svaante/dape"
                :fetcher github
                :files ("*.el")) 
            '';
            meta.description = "Emacs plugin for debugging with eglot, expirmental";
          };

      indent-bars =
        let
          rev = inputs.indent-bars.shortRev;
        in
          with pkgs;
          with pkgs.emacsPackages;
          melpaBuild {
            pname = "indent-bars";
            ename = "indent-bars";
            version = inputs.indent-bars.lastModifiedDate;
            commit = rev;
            packageRequires = [];

            src = fetchFromGitHub {
              inherit rev;
              owner = "jdtsmith";
              repo = "indent-bars";
              sha256 = inputs.indent-bars.narHash;
            };

            recipe = writeText "recipe" ''
              (indent-bars
                :repo "jdtsmith/indent-bars"
                :fetcher github
                :files ("*.el")) 
            '';
            meta.description = "Emacs plugin for indentation guides";
          };
          
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
        rg # grep, but superfast
        dired-single # Reuse the dired buffer
        direnv # Environment switcher for Emacs
        editorconfig # EditorConfig Emacs Plugin
        eldoc # Show function arglist or variable docstring in echo area
        magit # A Git porcelain inside Emacs
        forge # Extension for magit for handling forges
        nix-mode # Nix integration
        org # For keeping notes, maintaining TODO lists, and project planning
        org-modern # A fresh look for org
        org-drill # A spaced repetition system for Emacs
        org-pomodoro # Pomodoro technique implementation
        org-roam # A note-taking tool based on the principles of networked thought
        org-roam-ui # A graphical user interface for org-roam
        rainbow-delimiters # Make sense of the madness
        tabspaces # A space for the projects
        vterm # It's no kitty
        multi-vterm # More of them...
        flycheck # See the errors

        # Completion
        marginalia # A minibuffer completion UI
        consult # Consulting the minibuffer
        orderless # A completion style
        vertico # Vertically oriented completion
        corfu # Completion overlay

        # Theme
        modus-themes # Great themes

        # Language Server
        dap-mode # Debug Adapter Protocol mode

        # Programming language packages.
        json-mode # Major mode for editing JSON files
        vue-mode # Major mode for editing Vue.js files
        yaml-mode # Major mode for editing YAML files
        clojure-mode # Major mode for editing clojure files
        cider # Extends clojure-mode with superpowers
        web-mode # Major mode for editing web templates
        scala-ts-mode # Major mode for editing Scala files
        eglot-java # Better java support in eglot
        treesit-grammars.with-all-grammars # Now I see whats wrong
        dape # Can you find the bugs? maybe with this?
        # copilot # Copilot support

        # User interface packages.
        mood-line # A minimal modeline for emacs
        indent-bars # Indentation guides that work!? In my editor?!
      ];

    extraConfig = builtins.readFile ./init.el;
      
  };

  home.packages = with pkgs; [
    rnix-lsp
  ];

  services.emacs.enable = true;

}
