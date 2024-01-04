{ inputs, pkgs, ... }:

{
  programs.emacs = {
    enable = true;
    overrides = self: super: {
      # I install the packages below by hand because they're not in MELPA, and I
      # don't want to incur the startup cost of using straight.el.

      scala-cli-repl =
        let
          rev = inputs.scala-cli-repl.shortRev;
        in
          with pkgs;
          with pkgs.emacsPackages;
          melpaBuild {
            pname = "scala-cli-repl";
            ename = "scala-cli-repl";
            version = inputs.scala-cli-repl.lastModifiedDate;
            commit = rev;
            packageRequires = [];

            src = fetchFromGitHub {
              inherit rev;
              owner = "ag91";
              repo = "scala-cli-repl";
              sha256 = inputs.scala-cli-repl.narHash;
            };

            recipe = writeText "recipe" ''
              (scala-cli-repl
                :repo "ag91/scala-cli-repl"
                :fetcher github
                :files ("*.el")) 
            '';
            meta.description = "Emacs plugin for running scala-cli as a repl in emcacs";
          };


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

        # Optional packages.
        rg
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
        forge # Extension for magit for handling forges
        nerd-icons # Nerd icons for Emacs
        nix-mode # Nix integration
        org # For keeping notes, maintaining TODO lists, and project planning
        org-modern # A fresh look for org
        org-drill # A spaced repetition system for Emacs
        org-pomodoro # Pomodoro technique implementation
        org-roam # A note-taking tool based on the principles of networked thought
        org-roam-ui # A graphical user interface for org-roam
        pretty-mode # Redisplay parts of the buffer as pretty symbols
        rainbow-delimiters
        tabspaces
        vterm
        multi-vterm

        # Theme
        modus-themes

        # Language Server
        dap-mode # Debug Adapter Protocol mode

        # Programming language packages.
        company # Modular text completion framework
        company-quickhelp # Modular text completion framework
        helm-xref # Helm UI for xref
        json-mode # Major mode for editing JSON files
        vue-mode # Major mode for editing Vue.js files
        yaml-mode # Major mode for editing YAML files
        yasnippet # Template system for Emacs
        clojure-mode # Major mode for editing clojure files
        cider # Extends clojure-mode with superpowers
        web-mode # Major mode for editing web templates
        scala-ts-mode # Major mode for editing Scala files
        eglot-java # Better java support in eglot
        treesit-grammars.with-all-grammars
        # copilot # Copilot support

        # User interface packages.
        counsel # Various completion functions using Ivy
        git-gutter # A gutter for my git
        git-gutter-fringe # Some more guttering
        mood-line # A minimal modeline for emacs
        indent-bars # Indentation guides that work!? In my editor?!
        dape # Can you find the bugs? maybe with this?
        scala-cli-repl # Now what does this function return? let's REPL
      ];

    extraConfig = builtins.readFile ./init.el;
      
  };

  home.packages = with pkgs; [
    rnix-lsp
  ];

  services.emacs.enable = true;

}
