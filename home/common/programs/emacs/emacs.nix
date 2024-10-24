{ inputs, pkgs, ... }:
let

  metals =
    { version, outputHash }:
    let
      metalsDeps = pkgs.stdenv.mkDerivation {
        name = "metals-deps-${version}";
        buildCommand = ''
          export COURSIER_CACHE=$(pwd)
          ${pkgs.coursier}/bin/cs fetch org.scalameta:metals_2.13:${version} \
            -r bintray:scalacenter/releases \
            -r sonatype:snapshots > deps
          mkdir -p $out/share/java
          cp -n $(< deps) $out/share/java/
        '';
        outputHashMode = "recursive";
        outputHashAlgo = "sha256";
        inherit outputHash;
      };
    in
    pkgs.metals.overrideAttrs (old: {
      inherit version;
      extraJavaOpts = old.extraJavaOpts + " -Dmetals.client=nvim-lsp";
      buildInputs = [ metalsDeps ];
      jre = pkgs.temurin-bin-21;
    });

  metals-pkg = metals {
    version = "1.3.5";
    outputHash = "sha256-86/zeoOO5kSAwh7uQTV7nGUGQoIux1rlH5eUgvn3kvY=";
  };
in
{
  programs.emacs = {
    enable = true;
    overrides = self: super: {
      # I install the packages below by hand because they're not in MELPA, and I
      # don't want to incur the startup cost of using straight.el.

      indent-bars =
        let
          rev = inputs.indent-bars.shortRev;
        in
        with pkgs.emacsPackages;
        melpaBuild {
          pname = "indent-bars";
          ename = "indent-bars";
          version = inputs.indent-bars.lastModifiedDate;
          commit = rev;
          packageRequires = [ ];

          src = pkgs.fetchFromGitHub {
            inherit rev;
            owner = "jdtsmith";
            repo = "indent-bars";
            sha256 = inputs.indent-bars.narHash;
          };

          recipe = pkgs.writeText "recipe" ''
            (indent-bars
              :repo "jdtsmith/indent-bars"
              :fetcher github
              :files ("*.el")) 
          '';
          meta.description = "Emacs plugin for indentation guides";
        };

    };

    extraPackages =
      epkgs: with epkgs; [
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

        indent-bars

        # Completion
        marginalia # A minibuffer completion UI
        consult # Consulting the minibuffer
        orderless # A completion style
        vertico # Vertically oriented completion
        company # Completion overlay

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
        scala-ts-mode # Major mode for  Scala tree-sitter
        kotlin-ts-mode # Major mode for  Kotlin tree-sitter
        treesit-grammars.with-all-grammars # Now I see whats wrong
        lsp-mode # lsp for emacs
        yasnippet # snippets
        lsp-metals # Scala lsp
        lsp-java # So I can boilerplate faster
        lsp-ui # Prettier lsp stuff

        # User interface packages.
        # mood-line # A minimal modeline for emacs
      ];

    extraConfig = builtins.readFile ./init.el;

  };

  home.packages = [
    metals-pkg
    pkgs.java-language-server
  ];

  services.emacs.enable = true;

}
