{ pkgs }:

with pkgs;
let
  modifier = "Mod4";

  swaylockScript = pkgs.writeShellScript "swaylock.sh" ''
          swaylock \
    	--screenshots \
    	--clock \
    	--indicator \
    	--indicator-radius 100 \
    	--indicator-thickness 7 \
    	--effect-blur 7x5 \
    	--effect-vignette 0.5:0.5 \
    	--ring-color bb00cc \
    	--key-hl-color 880033 \
    	--line-color 00000000 \
    	--inside-color 00000088 \
    	--separator-color 00000000 \
    	--grace 2 \
    	--fade-in 0.2
  '';

  home-display-settings = pkgs.writeShellScript "home-displays-keybind.sh" ''
    swaymsg output "\"HP Inc. HP E27u G4 CN41320QQB\"" mode 2560x1440@59.951Hz pos 1440 570 transform normal scale 1.0 scale_filter nearest adaptive_sync off dpms on
    swaymsg output "\"HP Inc. HP E27u G4 CN41320QR1\"" mode 2560x1440@59.951Hz pos 0 0 transform 270 scale 1.0 scale_filter nearest adaptive_sync off dpms on
    swaymsg output "\"LG Display 0x06FA Unknown\"" dpms off
  '';

  laptop-display-settings = pkgs.writeShellScript "laptop-displays-keybind.sh" ''
    swaymsg output "\"LG Display 0x06FA Unknown\"" mode 1920x1080@60.014Hz pos 0 0 transform normal scale 1.0 scale_filter nearest adaptive_sync off dpms on
  '';

in
{
  "${modifier}+ctrl+e" = "exec emacsclient -c";
  "${modifier}+ctrl+t" = "exec emacsclient -c -e \"(eshell)\"";
  "${modifier}+alt+l" = "exec ${swaylockScript}";
  "${modifier}+ctrl+h" = "exec ${home-display-settings}";
  "${modifier}+ctrl+l" = "exec ${laptop-display-settings}";
  "${modifier}+ctrl+p" = "exec grimshot --notify save area";

  # Directional keybindings - focus
  "${modifier}+n" = "focus left";
  "${modifier}+e" = "focus down";
  "${modifier}+i" = "focus up";
  "${modifier}+o" = "focus right";

  # Layout
  "${modifier}+h" = "layout toggle split";
  
  # Directional keybindings - move windows
  "${modifier}+Shift+n" = "move left";
  "${modifier}+Shift+e" = "move down";
  "${modifier}+Shift+i" = "move up";
  "${modifier}+Shift+o" = "move right";
  
  # Workspace movement
  "${modifier}+Ctrl+n" = "workspace prev";
  "${modifier}+Ctrl+o" = "workspace next";

  # Resize
  "${modifier}+r" = "mode resize";
  
  # Volume control keybindings
  "XF86AudioRaiseVolume" = "exec pactl set-sink-volume @DEFAULT_SINK@ +5%";
  "XF86AudioLowerVolume" = "exec pactl set-sink-volume @DEFAULT_SINK@ -5%";
  "XF86AudioMute" = "exec pactl set-sink-mute @DEFAULT_SINK@ toggle";
  "XF86AudioMicMute" = "exec pactl set-source-mute @DEFAULT_SOURCE@ toggle";

  # Brightness control using brightnessctl
  "XF86MonBrightnessUp" = "exec brightnessctl set +5%";
  "XF86MonBrightnessDown" = "exec brightnessctl set 5%-";
  
} 
