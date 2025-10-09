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
  
  jjModeCommit = "bc8f74306c5a21cbfb4d5dc1b1263108b2a54f15";
  jjModeSrcHash = "sha256-fBZLDdlPTFE68rUNsQUvUC1VqjiyGctQCFg0c8h6e34=";

  jj-mode = pkgs.emacsPackages.melpaBuild {
    pname = "jj-mode";
    version = "0.0.0";
    commit = jjModeCommit;
    src = pkgs.fetchFromGitHub {
      owner = "bolivier";
      repo  = "jj-mode.el";
      rev   = jjModeCommit;
      hash  = jjModeSrcHash;
    };
    recipe = pkgs.writeText "recipe" ''
      (jj-mode
       :repo "bolivier/jj-mode.el"
       :fetcher github
       :files ("*.el"))
      '';
    packageRequires = [ pkgs.emacsPackages.magit ];
  };
  
in
{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs-pgtk;
    extraPackages =
      epkgs: with epkgs; [
        jj-mode
        orderless
        vertico
        marginalia
        embark
        embark-consult
        consult
        consult-project-extra
        corfu
        cape
        yasnippet
        wgrep
        avy
        nerd-icons
        indent-bars
        delight
        nerd-icons-completion
        nerd-icons-corfu
        spacious-padding
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
        iedit
        apheleia # Formatting
        citeproc
        denote
        denote-org
        consult-denote
        visual-fill-column
        eldoc-box
        vc-jj
      ];

    extraConfig = builtins.readFile ./init.el;
  };

  home.packages = [
    pkgs.jdt-language-server
    metals
    pkgs.astyle
    pkgs.fd
  ];

  home.file = {
    ".emacs.d" = {
      source = ./.;
      recursive = true;
    };

  };

  services.emacs.enable = true;

}
