{ pkgs, ... }:
{
  home.packages = [
    pkgs.niri
  ];

  xdg.configFile."niri/config.kdl".source = ./config.kdl;
}
