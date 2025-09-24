{ pkgs,lib,... }:

let
  kanagawa = import ./kanagawa-theme.nix;
in
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
          xkb_options = "grp:rctrl_toggle,ctrl:swapcaps_group2";
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
        border = 2;
      };

      gaps = {
        smartBorders = "on";
        smartGaps = false;
        inner = 3;
        outer = 2;
      };

      bars = [ ];

      startup = [ { command = "waybar"; } ];
      keybindings = pkgs.lib.mkOptionDefault (import ../sway-keybindings.nix { inherit pkgs; });
      
      # Kanagawa colors
      colors = {
        focused = {
          border = kanagawa.accentYellow;
          background = kanagawa.background;
          text = kanagawa.foreground;
          indicator = kanagawa.accentGreen;
          childBorder = kanagawa.accentYellow;
        };
        focusedInactive = {
          border = kanagawa.border;
          background = kanagawa.background;
          text = kanagawa.muted;
          indicator = kanagawa.border;
          childBorder = kanagawa.border;
        };
        unfocused = {
          border = kanagawa.border;
          background = kanagawa.background;
          text = kanagawa.muted;
          indicator = kanagawa.border;
          childBorder = kanagawa.border;
        };
        urgent = {
          border = kanagawa.accentRed;
          background = kanagawa.background;
          text = kanagawa.accentRed;
          indicator = kanagawa.accentRed;
          childBorder = kanagawa.accentRed;
        };
        placeholder = {
          border = kanagawa.border;
          background = kanagawa.background;
          text = kanagawa.muted;
          indicator = kanagawa.border;
          childBorder = kanagawa.border;
        };
        background = kanagawa.background;
      };
      
      fonts = {
        names = [ kanagawa.font ];
        size = (lib.toInt (builtins.toString kanagawa.fontSize) - 2) * 1.0;
      };
    };

    extraConfig = ''
    shadows enable
    corner_radius 10
    
    # SwayFX specific effects with Kanagawa colors
    layer_effects "panel" {
        shadows enable;
        corner_radius 20;
    }
    
    # Window shadows for SwayFX
    shadows_on_csd enable
    shadow_blur_radius 20
    shadow_color ${kanagawa.background}ee
    
    # Blur effects (optional, you can remove if you don't want blur)
    blur enable
    blur_xray disable
    blur_passes 2
    blur_radius 5
    
    # Dim inactive windows slightly
    default_dim_inactive 0.1
    dim_inactive_colors.unfocused ${kanagawa.background}ee
    dim_inactive_colors.urgent ${kanagawa.accentRed}ee
    '';
  };
}
