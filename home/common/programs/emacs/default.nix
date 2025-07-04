{ pkgs, ... }:
let
  metalsVersion = "1.5.1";
  metals = pkgs.metals.overrideAttrs (
    final: prev: {
      deps = pkgs.stdenv.mkDerivation {
        name = "${prev.pname}-deps-${metalsVersion}";
        buildCommand = ''
          export COURSIER_CACHE=$(pwd)
          ${pkgs.pkgs.coursier}/bin/cs fetch org.scalameta:metals_2.13:${metalsVersion} \
            -r bintray:scalacenter/releases \
            -r sonatype:snapshots > deps
          mkdir -p $out/share/java
          cp $(< deps) $out/share/java/
        '';
        outputHashMode = "recursive";
        outputHashAlgo = "sha256";
        outputHash = "sha256-2FA2B/WzNGU4B785pn5zZ9Xj64huzbSbr2Or+CxUMlI=";
      };
      buildInputs = [ final.deps ];
    }
  );

  haskell-ts-mode-hash = "95388599a3b5ca33342d4445ebcae3094068c151";
  haskell-ts-mode-custom = pkgs.emacsPackages.melpaBuild {
    pname = "haskell-ts-mode";
    version = "1";
    commit = haskell-ts-mode-hash;

    src = pkgs.fetchgit {
      url = "https://codeberg.org/pranshu/haskell-ts-mode.git";
      rev = haskell-ts-mode-hash;
      sha256 = "sha256-bhMx+8oy08bRLm6HepoKbDxa3mQS5Tv/R94vVdZRqM4="; 
    };

    recipe = pkgs.writeText "recipe" ''
      (haskell-ts-mode :fetcher git :url "https://codeberg.org/pranshu/haskell-ts-mode.git")
    '';
  };

  tree-sitter-unison = pkgs.tree-sitter.buildGrammar {
    language = "tree-sitter-unison";
    version = "2.0.1";
    src = pkgs.fetchFromGitHub {
      owner = "kylegoetz";
      repo = "tree-sitter-unison";
      rev = "169e7f748a540ec360c0cb086b448faad012caa4";
      hash = "sha256-0HOLtLh1zRdaGQqchT5zFegWKJHkQe9r7DGKL6sSkPo=";
    };
  };
  
  unison-ts-mode = pkgs.emacsPackages.melpaBuild {
    pname = "unison-ts-mode";
    version = "1";
    commit = "04cbd1f73f94346e68f9b42f8ab9d7ab8ab43ad3";

    src = pkgs.fetchgit {
      url = "https://github.com/fmguerreiro/unison-ts-mode";
      rev = "04cbd1f73f94346e68f9b42f8ab9d7ab8ab43ad3";
      sha256 = "sha256-GwF5//vnrdANGWz8gDv7Oi79UDGej88VXtnalV85f6o="; 
    };

      recipe = pkgs.writeText "recipe" ''
      (unison-ts-mode :fetcher git :url "https://github.com/fmguerreiro/unison-ts-mode")
      '';
    };

    eglot-booster = pkgs.emacsPackages.melpaBuild {
      pname = "eglot-booster";
      version = "20241029";

      commit = "e6daa6bcaf4aceee29c8a5a949b43eb1b89900ed";

      src = pkgs.fetchFromGitHub {
        owner = "jdtsmith";
        repo = "eglot-booster";
        rev = "e6daa6bcaf4aceee29c8a5a949b43eb1b89900ed";
        hash = "sha256-PLfaXELkdX5NZcSmR1s/kgmU16ODF8bn56nfTh9g6bs=";
      };

      recipe = pkgs.writeText "recipe" ''
        (eglot-booster
        :repo "jdtsmith/eglot-booster"
        :fetcher github
        :files ("*.el"))
        ''; 
      };
in
{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs-pgtk;
    extraPackages =
      epkgs: with epkgs; [
        # Meow!
        meow

        # Native compilation
        # compile-angel

        # Completion et.al.
        orderless
        vertico
        marginalia
        embark
        embark-consult
        consult
        corfu
        cape
        yasnippet
        wgrep

        # Good looks
        nerd-icons
        indent-bars
        ef-themes
        vim-tab-bar
        mood-line
        nerd-icons-completion
        nerd-icons-corfu

        # Direnv
        envrc

        # Git
        magit
        forge
        git-gutter-fringe
        fringe-helper
        git-gutter

        # Programming
        (treesit-grammars.with-grammars (grammars: with grammars; [
          tree-sitter-nix
          tree-sitter-unison
          tree-sitter-haskell
          tree-sitter-scala
          tree-sitter-java
          tree-sitter-yaml
          tree-sitter-typescript
          tree-sitter-javascript
          tree-sitter-vue
          tree-sitter-tsx
        ]))
        scala-ts-mode
        nix-ts-mode
        web-mode
        fsharp-mode
        eglot-fsharp
        eglot-java
        haskell-ts-mode-custom
        unison-ts-mode
        dape

        # Editing
        markdown-mode
        iedit
        apheleia # Formatting

        # Other
        eat
        org-roam
        org-roam-ui
        org-modern
        tabspaces
        detached
        dashboard

        # lsp
        # eglot-booster
        eldoc-box
      ];

    extraConfig = builtins.readFile ./init.el;
  };

  home.packages = [
    pkgs.emacs-lsp-booster
    pkgs.jdt-language-server
    metals
    pkgs.scalafmt
    pkgs.astyle
    pkgs.fd
    pkgs.fsautocomplete
    pkgs.dtach
  ];

  home.file = {
    ".emacs.d" = {
      source = ./.;
      recursive = true;
    };

  };

  services.emacs.enable = true;

}
