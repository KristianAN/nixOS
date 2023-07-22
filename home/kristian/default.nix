{
  pkgs,
  lib,
  ...
}: {
#  imports = [
#    ../common
#     ./desktop
#     ./services
#  ];

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  home = {
    username = "kristian";
    homeDirectory = "/home/kristian";
  };

  home.packages = with pkgs; [
    dconf
    wofi
    waybar
    wdisplays
  ];

 programs.home-manager.enable = true;

 programs.tmux = {
    enable = true;
    extraConfig = ''
      seg -g escape time 0
      seg -g prefix C-s
      unbind C-b
      bind C-s send-prefix
      seg -g base-index 1
      seg-option -ga terminal-overrides ",xterm-256color:Tc"
    '';
  };

  programs.git = {
    enable = true;
    package = pkgs.gitAndTools.gitFull;
    userEmail = "kristian@krined.no";
    userName = "Kristian Nedrevold-Hansen";
    extraConfig = {
      core.editor = "nvim";
    };
  };


  programs.neovim = {
    enable = true;
    vimAlias = true;
    extraLuaConfig = lib.fileContents ./../config/neovim/init.lua;
    extraPackages = with pkgs; [
      gcc
      fzf
      cmake
    ];
  };

  programs.kitty = {
    enable = true;
    font = {
      name = "Iosevka";
      size = 12;
    };
  };

  # Setup Sway
  wayland.windowManager.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
    # Sway config
    config = {
      terminal = "kitty";
      menu = "wofi --show run";
      modifier = "Mod4";

      input = {
        "*" = {
          xkb_layout = "us,no";
          xkb_options = "caps:escape,grp:rctrl_toggle";
        };
      };

      output = {
        eDP-1 = {
          scale = "1";
          mode = "1920x1080";
        };
      };

      window = {
        titlebar = false;
      };

      gaps = {
        smartBorders = "on";
        smartGaps = true;
        inner = 5;
        outer = 2;
      };

      bars = [];

      startup = [
        {command = "waybar";}
      ];
    };
  };

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = "23.05";
}
