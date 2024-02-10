{ pkgs
, lib
, inputs
, ...
}: {

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

  home.packages = with pkgs; [
    dconf
    wdisplays
    wev
    swaylock-effects
    dunst
    inputs.myNeovimFlake.packages.x86_64-linux.nvim
  ];

  programs.home-manager.enable = true;

  home.sessionVariables.EDITOR = "nvim";

  programs.gh = {
    enable = true;
  };

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = "23.05";
}
