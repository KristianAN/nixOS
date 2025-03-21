{ pkgs, ... }:
{
  wayland.windowManager.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
    checkConfig = false;
    # Sway config
    config = {
      terminal = "wezterm";
      menu = "fuzzel";
      modifier = "Mod4";

      input = {
        "*" = {
          xkb_layout = "no,us";
          xkb_variant = ",colemak_dh_iso";
          xkb_options = "grp:rctrl_toggle";
          tap = "enabled";
        };
      };

      output = {
        "*" = {
          bg = "~/Pictures/kanagawa-wp.jpg fill";
        };
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

      bars = [ ];

      startup = [ { command = "yambar"; } ];
      keybindings = pkgs.lib.mkOptionDefault (import ../sway-keybindings.nix { inherit pkgs; });
    };
  };

}
