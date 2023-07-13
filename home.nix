{ config, pkgs, ... };
let home-manager = builtins.fetchTarball "https://github.com/nix-community/home-manager/arhive/master.tar.gz";
in
{
  imports = [
    (import "${home-manager}/nixos")
  ];
  home-manager.users.kristian = {
    home.stateVersion = "18.09";
    };
}
