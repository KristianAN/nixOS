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
    ripgrep
    xdotool
    pstree
    unzip
    wl-clipboard
    acpi
    btop
    fd
    fzf
    neofetch

    # Sound stuff. TODO move to other file
    pulseaudio # Make sure this isn't enabled
    pavucontrol
    
    # Archive tools
    xarchiver
    wget
    curl
    zip
    git
    zsh
  ];

  users.users.kristian.shell = pkgs.zsh;
  programs.zsh.enable = true;
  
  imports = [
    ./fonts.nix
  ];
}