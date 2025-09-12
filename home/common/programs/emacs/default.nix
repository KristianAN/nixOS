{ pkgs, ... }:
let
  metalsVersion = "1.6.2";
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
        outputHash = "sha256-WcPgX0GZSqpVVAzQ1zCxuRCkwcuR/8bwGjSCpHneeio=";
      };
      buildInputs = [ final.deps ];
    }
  );

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
        delight
        nerd-icons-completion
        nerd-icons-corfu
        spacious-padding
        ef-themes
        
        # Direnv
        envrc

        # Programming
        (treesit-grammars.with-grammars (grammars: with grammars; [
          tree-sitter-nix
          tree-sitter-bash
          tree-sitter-haskell
          tree-sitter-scala
          tree-sitter-java
          tree-sitter-yaml
          tree-sitter-typescript
          tree-sitter-javascript
          tree-sitter-vue
          tree-sitter-tsx
          tree-sitter-css
        ]))
        scala-ts-mode
        nix-ts-mode
        web-mode
        haskell-ts-mode

        # Editing
        iedit
        apheleia # Formatting
        citeproc
        
        # Other
        eat
        denote
        denote-org
        consult-denote
        org-present
        visual-fill-column
        detached

        # lsp
        eldoc-box
      ];

    extraConfig = builtins.readFile ./init.el;
  };

  home.packages = [
    pkgs.emacs-lsp-booster
    pkgs.jdt-language-server
    metals
    pkgs.astyle
    pkgs.fd
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
