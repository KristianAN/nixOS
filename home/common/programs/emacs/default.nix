{ pkgs, ... }:
let
  metalsVersion = "1.6.4";
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
        outputHash = "sha256-MuzyVyTOVWZjs+GPqrztmEilirRjxF9SJIKyxgicbXM=";
      };
      buildInputs = [ final.deps ];
    }
  );

in
{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs-pgtk;
    extraPackages =
      epkgs: with epkgs; [
        magit
        rust-mode
        orderless
        vertico
        marginalia
        embark
        embark-consult
        consult
        consult-hoogle
        corfu
        cape
        yasnippet
        wgrep
        avy
        dape
        nerd-icons
        indent-bars
        nerd-icons-completion
        nerd-icons-corfu
        spacious-padding
        envrc
        # Programming
        (treesit-grammars.with-grammars (grammars: with grammars; [
          tree-sitter-nix
          tree-sitter-rust
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
        iedit
        apheleia # Formatting
        citeproc
        denote
        denote-org
        consult-denote
        visual-fill-column
        eldoc-box
        meow
        doom-modeline
      ];

    extraConfig = builtins.readFile ./init.el;
  };

  home.packages = [
    pkgs.jdt-language-server
    metals
    pkgs.astyle
    pkgs.fd
    # pkgs.haskellPackages.hoogle
  ];

  home.file = {
    ".emacs.d" = {
      source = ./.;
      recursive = true;
    };

  };

  services.emacs.enable = true;

}
