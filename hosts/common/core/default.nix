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
    filezilla
    ncdu
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
    htop
    killall
    tree
    dbeaver

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

    # Keyring
    gnome.gnome-keyring

    

  ];

  users.users.kristian.shell = pkgs.zsh;

  security.pam.services.swaylock = {};

  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --cmd sway";
        user = "greeter";
      };
    };
  };

  programs.zsh.enable = true;

  systemd = {
    user.services.polkit-gnome-authentication-agent-1 = {
      description = "polkit-gnome-authentication-agent-1";
      wantedBy = [ "graphical-session.target" ];
      wants = [ "graphical-session.target" ];
      after = [ "graphical-session.target" ];
      serviceConfig = {
          Type = "simple";
          ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
          Restart = "on-failure";
          RestartSec = 1;
          TimeoutStopSec = 10;
        };
    };
  };

  virtualisation = {
    podman.enable = true;
    libvirtd.enable = true;
    docker = {
      enable = true;
      daemon.settings = {
        data-root = "/opt/docker";
      };
    };
  };
      
  imports = [
    ./fonts.nix
  ];
}
