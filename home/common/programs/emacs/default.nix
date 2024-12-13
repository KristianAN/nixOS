{ pkgs, ... }:
let
  metals = pkgs.metals.overrideAttrs (
    final: prev: {
      version = "1.4.1";
      deps = pkgs.stdenv.mkDerivation {
        name = "${prev.pname}-deps-1.4.1";
        buildCommand = ''
          export COURSIER_CACHE=$(pwd)
          ${pkgs.pkgs.coursier}/bin/cs fetch org.scalameta:metals_2.13:1.4.1 \
            -r bintray:scalacenter/releases \
            -r sonatype:snapshots > deps
          mkdir -p $out/share/java
          cp $(< deps) $out/share/java/
        '';
        outputHashMode = "recursive";
        outputHashAlgo = "sha256";
        outputHash = "sha256-CVAPjeTYuv0w57EK/IldJcGz8mTQnyCGAjaUf6La2rU";
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
    package = pkgs.emacs30;
    extraPackages =
      epkgs: with epkgs; [
        # Evil 
        evil
        evil-collection
        undo-fu
        undo-fu-session
        vim-tab-bar
        evil-visualstar
        evil-surround
        evil-leader

        # Native compilation
        # compile-angel

        # Completion et.al.
        orderless
        vertico
        marginalia
        embark
        embark-consult
        consult
        company
        yasnippet
        wgrep

        # Good looks
        nerd-icons
        cyberpunk-theme
        ef-themes
        indent-bars
        doom-modeline

        # Direnv
        envrc

        # Git
        magit

        # Project management
        projectile

        # Programming
        treesit-grammars.with-all-grammars
        scala-ts-mode
        nix-ts-mode
        haskell-mode
        eglot-booster
        eldoc-box
        web-mode

        # Editing
        markdown-mode
        apheleia

        # Other
        dirvish
        eat
      ];

    extraConfig = builtins.readFile ./init.el;
  };

  home.packages = [
    pkgs.emacs-lsp-booster
    metals
    pkgs.scalafmt
    pkgs.astyle
    pkgs.fd
    pkgs.imagemagick
    pkgs.poppler
    pkgs.ffmpegthumbnailer
    pkgs.mediainfo

  ];

  home.file = {
    ".emacs.d" = {
      source = ./.;
      recursive = true;
    };
  };

  services.emacs.enable = true;

}
