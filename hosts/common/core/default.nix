{
  pkgs,
  lib,
  ...
}:
with lib; {
  # Core pakages for system
  environment.systemPackages = with pkgs; [
    # Nix Core
    nix-index
    wgetpaste
    neofetch
    ripgrep
    xdotool
    pstree
    unzip
    xclip
    acpi
    btop
    fd

    # Archive tools
    xarchiver
    wget
    curl
    zip
    git
  ];

  imports = [
    ./fonts.nix
  ];
}
