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
    opengl = {
      enable = true;
    };
  };

  xdg = {
    portal = {
      enable = true;
      extraPortals = with pkgs; [
        xdg-desktop-portal-wlr
        xdg-desktop-portal-gtk
      ];
    };
  };

  # Remove if you wish to disable unfree packages for your system
  nixpkgs.config.allowUnfree = true;

  # NixOS release to be compatible with for staeful data such as databases.
  system.stateVersion = "24.05";
  services.flatpak.enable = true;
}
