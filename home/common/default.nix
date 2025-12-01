{ inputs, pkgs, ... }:
{

  imports = [
    ./services
    ./programs
  ];

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  home = {
    username = "kristian";
    homeDirectory = "/home/kristian";
  };

  home.packages = [
    pkgs.nixfmt-tree
    pkgs.ladybird
    pkgs.virt-manager
    pkgs.dconf
    pkgs.wdisplays
    pkgs.wev
    pkgs.swaylock-effects
    pkgs.digikam
    pkgs.wf-recorder
    pkgs.slurp
    pkgs.mpv
    pkgs.sway-contrib.grimshot
    pkgs.texliveFull
    pkgs.brightnessctl
    pkgs.neovim
    pkgs.pandoc
    pkgs.any-nix-shell
    pkgs.cmatrix
    pkgs.proton-pass
    pkgs.hunspell
    pkgs.hunspellDicts.en_GB-ise
    pkgs.hunspellDicts.nb_NO
    pkgs.hunspellDicts.nn_NO
    pkgs.playerctl
    inputs.hell.packages.${pkgs.system}.default
    pkgs.jq
  ];

  programs.home-manager.enable = true;

  home.sessionVariables.EDITOR = "emacsclient";

  programs.gh = {
    enable = true;
  };

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = "25.05";

}
