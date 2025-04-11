{ pkgs, ... }:
{
  wayland.windowManager.sway = {
    package = pkgs.swayfx;
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
          bg = "${./wallpaper.png} fill";
        };
        eDP-1 = {
          scale = "1";
          mode = "1920x1080";
        };
      };
      
      modes = {
        resize = {
          "m" = "resize shrink width 10px";
          "n" = "resize grow height 10px";
          "e" = "resize shrink height 10px";
          "i" = "resize grow width 10px";
          "Return" = "mode default";
          "Escape" = "mode default";
        };
      };
      
      window = {
        titlebar = false;
      };

      gaps = {
        smartBorders = "on";
        smartGaps = false;
        inner = 3;
        outer = 2;
      };

      bars = [ ];

      startup = [ { command = "yambar"; } ];
      keybindings = pkgs.lib.mkOptionDefault (import ../sway-keybindings.nix { inherit pkgs; });
    };

    # to find the app_id we can run: swaymsg -t get_tree
    extraConfig = ''
    for_window [app_id="emacs"] opacity 0.9
    for_window [app_id="firefox"] opacity 0.8
    for_window [app_id="fuzzel"] opacity 0.8
    for_window [app_id="org.wezfurlong.wezterm"] opacity 0.8
    for_window [class="Slack"] opacity 0.8
    blur enable
    blur_passes 3
    blur_radius 8
    shadows enable
    corner_radius 10
    
    layer_effects "panel" {
        blur enable;
        blur_xray enable;
        blur_ignore_transparent enable;
        shadows enable;
        corner_radius 20;
    }
    '';
  };

}
