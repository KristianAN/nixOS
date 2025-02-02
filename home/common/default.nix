{ pkgs, inputs, ... }:
{

  imports = [
    ./services
    ./programs
  ];

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;
  nix.package = pkgs.nixVersions.nix_2_25;

  home = {
    username = "kristian";
    homeDirectory = "/home/kristian";
  };

  home.packages = [
    pkgs.vscode
    pkgs.virt-manager
    pkgs.libreoffice
    pkgs.dconf
    pkgs.wdisplays
    pkgs.wev
    pkgs.swaylock-effects
    pkgs.dunst
    pkgs.digikam
    pkgs.wf-recorder
    pkgs.slurp
    pkgs.mpv
    pkgs.neovide
    pkgs.distrobox
    pkgs.sway-contrib.grimshot
    pkgs.texliveFull
    pkgs.brightnessctl
    inputs.myNeovimFlake.packages.x86_64-linux.nvim
    pkgs.pandoc
    pkgs.any-nix-shell
  ];

  programs.home-manager.enable = true;

  home.sessionVariables.EDITOR = "nvim";

  programs.gh = {
    enable = true;
  };

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = "24.05";

}
