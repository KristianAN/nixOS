{ config, pkgs, ... }:
let home-manager = builtins.fetchTarball "https://github.com/nix-community/home-manager/archive/release-23.05.tar.gz";
in
{
  imports = [
    (import "${home-manager}/nixos")
  ];
  home-manager.users.kristian = {
    programs.home-manager = { enable = true; };
    home.stateVersion = "23.05";

    home.packages = with pkgs; [
      htop
      neofetch
    ];

    programs.git = {
      enable = true;
      package = pkgs.gitAndTools.gitFull;
      userEmail = "kristian@krined.no";
      extraConfig = {
        core.editor = "nvim";};
    };

    programs.neovim = {
      enable = true;
      vimAlias = true;
    };

  };
}
