{ pkgs, ... }:
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
    pkgs.virt-manager
    pkgs.dconf
    pkgs.wdisplays
    pkgs.wev
    pkgs.swaylock-effects
    pkgs.dunst
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
  ];

  programs.home-manager.enable = true;

  home.sessionVariables.EDITOR = "emacsclient";

  programs.gh = {
    enable = true;
  };

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = "24.05";

}
