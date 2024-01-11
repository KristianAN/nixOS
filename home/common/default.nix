{
  pkgs,
  lib,
  inputs,
  ...
}: {

  imports = [
     ./services
     ./programs
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
    wdisplays
    wev
    swaylock-effects
    dunst
    inputs.myNeovimFlake.packages.x86_64-linux.nvim

  ];

  programs.home-manager.enable = true;

  home.sessionVariables.EDITOR = "${pkgs.emacs}/bin/emacsclient -c";

  programs.waybar = {
    enable = true;
  };

  programs.tmux = {
    enable = true;
    shell = "${pkgs.zsh}/bin/zsh";
    escapeTime = 0;
    prefix = "C-a"; # set prefix to ctrl + a
    clock24 = true; # 24 hour clock
    historyLimit = 100000; # increase history limit
    baseIndex = 1; # start window/pane index at 1
    sensibleOnTop = true; # use tmux-sensible
    disableConfirmationPrompt = true; # i know what i'm doing, kill without prompt
    terminal = "screen-256color";
    tmuxinator.enable = true;
    # set-option -g terminal-overrides ",xterm-256color:Tc"
    plugins = with pkgs.tmuxPlugins; [
      resurrect
      continuum
      vim-tmux-navigator
      better-mouse-mode
    ];
    extraConfig = ''
      set -g set-titles on
      set -g set-titles-string "#W #{command} #T #{session_path}"
      set -g focus-events on
      set -g set-clipboard on
      
      # enable using a mouse
      set -g mouse on

      # automatically renumber windows after one is closed
      set-option -g renumber-windows on

      # split panes with | and -
      bind | split-window -h -c "#{pane_current_path}"
      bind - split-window -v -c "#{pane_current_path}"
      unbind '"'
      unbind %

      set-option -g status-position bottom
      set-option -g status on
      set-option -g status-interval 1
      set-option -g automatic-rename on
      set-option -g automatic-rename-format '#{b:pane_current_path}'

      # tmuxline
      set -g status "on"
      set -g status-justify "left"
      set -g status-style "none,bg=default"
      set -g status-left-style "none"
      set -g status-left-length "100"
      set -g status-right-style "none"
      set -g status-right-length "100"
      set -g pane-border-style "fg=#2e3440,bg=default"
      set -g pane-active-border-style "fg=#3b4252,bg=default"
      set -g pane-border-status bottom
      set -g pane-border-format ""
      set -g message-style "fg=brightwhite,bg=default"
      set -g message-command-style "fg=brightwhite,bg=default"
      setw -g window-status-activity-style "none"
      setw -g window-status-separator ""
      setw -g window-status-style "none,fg=brightwhite,bg=default"
      set -g status-left "#[fg=#8fbcbb,bg=default,bold]#S #[fg=brightwhite,bg=default,nobold,nounderscore,noitalics]"
      set -g status-right "#[fg=#616E88,bg=default]%k:%M #[fg=#616E88,bg=default] %Y-%m-%d "
      setw -g window-status-format "#[fg=#616E88,bg=default] #I#[fg=#616E88,bg=default] #W "
      setw -g window-status-current-format "#[fg=#5e81ac,bg=default] #I#[fg=brightwhite,bg=default] #W "
    '';
  };

  programs.git = {
    enable = true;
    package = pkgs.gitAndTools.gitFull;
    userEmail = "kristian@krined.no";
    userName = "Kristian Nedrevold-Hansen";
    extraConfig = {
      core.editor = "emacsclient -c";
      github.user = "KristianAN";
    };
    ignores = [
        "*~"
        "\#*\#"
        "/.emacs.desktop"
        "/.emacs.desktop.lock"
        "*.elc"
        "auto-save-list"
        "tramp"
        ".\#*"
        ".org-id-locations"
        "*_archive"
        "*_flymake.*"
        "*.rel"
        "flycheck_*.el"
        ".projectile"
        ".dir-locals.el"
    ];
  };

  programs.neovim = 
  let
    servers = with pkgs; { 
      jdtls = writeShellScriptBin "jdtls" "${jdt-language-server}/bin/jdt-language-server $*"; 
      hls = haskell-language-server;
      nixd = nixd;
      tsserver = nodePackages.typescript-language-server;
      volar = nodePackages.volar;
      rust = rust-analyzer;
    };
  in {
    enable = false;
    vimAlias = true;
    extraLuaConfig = lib.fileContents ./../config/neovim/init.lua;
    extraPackages = with pkgs; [
      gcc
      fzf
      cmake
    ] ++ builtins.attrValues servers;
  };

  programs.kitty = {
    enable = true;
    font = {
      name = "Iosevka Nerd Font";
      size = 13;
    };
    extraConfig = ''

# vim:ft=kitty

## name: Kanagawa
## license: MIT
## author: Tommaso Laurenzi
## upstream: https://github.com/rebelot/kanagawa.nvim/


background #1F1F28
foreground #DCD7BA
selection_background #2D4F67
selection_foreground #C8C093
url_color #72A7BC
cursor #C8C093

# Tabs
active_tab_background #1F1F28
active_tab_foreground #C8C093
inactive_tab_background  #1F1F28
inactive_tab_foreground #727169
#tab_bar_background #15161E

# normal
color0 #16161D
color1 #C34043
color2 #76946A
color3 #C0A36E
color4 #7E9CD8
color5 #957FB8
color6 #6A9589
color7 #C8C093

# bright
color8  #727169
color9  #E82424
color10 #98BB6C
color11 #E6C384
color12 #7FB4CA
color13 #938AA9
color14 #7AA89F
color15 #DCD7BA


# extended colors
color16 #FFA066
color17 #FF5D62
    '';
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
        emc = "emacsclient -t";
      };
      oh-my-zsh = {
        enable = true;
        plugins = [ "git" "systemd" "rsync" "kubectl" ];
        theme = "half-life";
      };
    };

  programs.vscode = {
    enable = true;
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
          xkb_layout = "no,us";
          xkb_variant= ",colemak_dh_iso";
          xkb_options = "grp:rctrl_toggle"; 
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
        { command = "waybar"; }
        { command = "flameshot"; }
      ];
      keybindings = lib.mkOptionDefault (import ./keybindings.nix { inherit pkgs; });
    };
  };

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = "23.05";
}
