{ pkgs, lib, osConfig, ... }:
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
    extraPackages = epkgs:
      (import ./packages.nix epkgs)
      ++ lib.optionals osConfig.programs.ewm.enable [ osConfig.programs.ewm.ewmPackage ];

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

  services.emacs.enable = !osConfig.programs.ewm.enable;

}
