{
  pkgs,
  modulesPath,
  ...
}:
{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    ../common
    ./boot.nix
    ./hardware.nix
    ./network.nix
    ./nix.nix
    ./system.nix
    ./user.nix
  ];

  hardware = {
    graphics = {
      enable = true;
    };
  };

  xdg = {
    portal = {
      enable = true;
      config.common.default = "*";
      extraPortals = with pkgs; [
        xdg-desktop-portal-gnome
        xdg-desktop-portal-wlr
      ];
    };
  };

  # Remove if you wish to disable unfree packages for your system
  nixpkgs.config.allowUnfree = true;

  # NixOS release to be compatible with for staeful data such as databases.
  system.stateVersion = "24.05";
  services.flatpak.enable = true;
}
