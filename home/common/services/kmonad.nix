# home/kristian/kmonad.nix
{ config, pkgs, ... }:
{
  home.packages = [
    pkgs.kmonad
  ];

  xdg.configFile."kmonad/config.kbd".source = ./config.kbd;

  systemd.user.services.kmonad-laptop = {
    Unit = {
      Description = "KMonad internal keyboard (user session)";
      After = [ "graphical-session.target" ];
    };
    Service = {
      # Use ${config.xdg.configHome} for robustness
      ExecStart = "${pkgs.kmonad}/bin/kmonad ${config.xdg.configHome}/kmonad/config.kbd";
      Restart = "on-failure";
      # (Optional) Add some environment for debugging:
      # Environment = "RUST_LOG=debug";
    };
    Install = {
      WantedBy = [ "default.target" ];
    };
  };
}
