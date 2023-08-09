{
  pkgs,
  lib,
  ...
}: {

  imports = [
     ./services
  ];

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
    wev
    swaylock-effects
  ];

 programs.home-manager.enable = true;

 programs.tmux = {
    enable = true;
    shell = "${pkgs.zsh}/bin/zsh";
    escapeTime = 0;
    # set-option -g terminal-overrides ",xterm-256color:Tc"
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

  programs.zsh = {
      enable = true;
      enableCompletion = false; # enabled in oh-my-zsh
      initExtra = ''
        test -f ~/.dir_colors && eval $(dircolors ~/.dir_colors)
      '';
      shellAliases = {
        please = "sudo";
        nd = "nix develop";
        tmr = "tmux attach-session -d";
      };
      oh-my-zsh = {
        enable = true;
        plugins = [ "git" "systemd" "rsync" "kubectl" ];
        theme = "half-life";
      };
    };

  programs.firefox = {
    enable = true;
  };

  gtk = {
    enable = true;
    iconTheme = {
      package = pkgs.breeze-icons;
      name = "breeze-dark";
    };
    theme = {
      package = pkgs.breeze-gtk;
      name = "Breeze-Dark";
    };
    gtk4.extraConfig = {
      gtk-application-prefer-dark-theme = true;
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
          xkb_options = "caps:escape,grp:rctrl_toggle,ctrl:swap_lalt_lctl"; 
          tap = "enabled";
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
      keybindings = lib.mkOptionDefault (import ./keybindings.nix { inherit pkgs; });
    };
  };

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = "23.05";
}
