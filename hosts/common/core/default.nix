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
    wl-clipboard
    acpi
    btop
    fd
    fzf

    # Sound stuff. TODO move to other file
    pulseaudio # Make sure this isn't enabled
    pavucontrol
    
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
