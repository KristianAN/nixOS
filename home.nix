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
      # Sway #
      wl-clipboard
      wofi
      waybar
      wdisplays
      # End Sway #
      # Fonts
      (pkgs.nerdfonts.override { 
        fonts = ["FiraCode" "Iosevka" "Ubuntu"];
      })

    ];

    fonts.fontconfig.enable = true;

    programs.kitty = {
      enable = true;
      font = {
        name = "Iosevka";
	size = 12;
      };
    };

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

    # Setup Sway
    wayland.windowManager.sway = {
      enable = true;
      wrapperFeatures.gtk = true;
      # Sway config
      config = {
        terminal = "kitty";
	menu = "wofi --show run";
	modifier = "Mod4";

	bars = [{
	  fonts.size = 15.0;
	  position = "top";
	}];

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
      };
    };
  };

}
