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
        ef-themes
        indent-bars
        doom-modeline

        # Direnv
        envrc

        # Git
        magit
        git-gutter-fringe
        git-gutter

        # Programming
        treesit-grammars.with-all-grammars
        scala-ts-mode
        nix-ts-mode
        haskell-ts-mode
        eglot-booster
        eldoc-box
        web-mode

        # Editing
        markdown-mode
        apheleia

        # Other
        dirvish
        eat
        org-roam
        org-modern
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
