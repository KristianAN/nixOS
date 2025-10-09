{ lib, ... }:

let
  kanagawa = import ./../kanagawa-theme.nix;

  # Tune these:
  sideMarginPixels = 12;   # distance from screen edge to bar background
  topMarginPixels  = 12;   # vertical offset from top
  innerPadX        = 24;   # horizontal padding inside the bar background
in
{
  programs.waybar.enable = true;
  xdg.configFile."waybar/config".source = ./config;

  xdg.configFile."waybar/style.css".text = lib.concatStringsSep "\n" [
    "/* Waybar Kanagawa Embedded Theme */"
    "@define-color fg ${kanagawa.foreground};"
    "@define-color accent_yellow ${kanagawa.accentYellow};"
    "@define-color accent_red ${kanagawa.accentRed};"
    "@define-color accent_green ${kanagawa.accentGreen};"
    "@define-color accent_blue ${(kanagawa.accentBlue or kanagawa.accentYellow)};"
    "@define-color accent_magenta ${(kanagawa.accentMagenta or kanagawa.accentRed)};"
    "@define-color accent_orange ${(kanagawa.accentOrange or kanagawa.accentYellow)};"
    "@define-color bg_base rgba(42,42,55,0.78);"
    "@define-color bg_surface rgba(48,48,62,0.62);"
    "@define-color bg_alt rgba(62,62,78,0.50);"

    "window#waybar {"
    "  margin: ${toString topMarginPixels}px ${toString sideMarginPixels}px 0 ${toString sideMarginPixels}px;"
    "  padding: 6px ${toString innerPadX}px;"
    "  border-radius: 18px;"
    "  background: linear-gradient(145deg, rgba(34,34,46,0.85), rgba(52,52,66,0.70));"
    "  border: 1px solid rgba(255,255,255,0.05);"
    "}"

    "* {"
    "  font-family: \"${kanagawa.font}\", \"Font Awesome 6 Free\", sans-serif;"
    "  font-size: ${builtins.toString kanagawa.fontSize}px;"
    "  font-weight: 500;"
    "  color: @fg;"
    "  background: transparent;"
    "}"

    "/* Pill modules (now excluding child modules of desk & system) */"
    "#desk, #clock, #mpris, system, #system {"
    "  padding: 4px 14px;"
    "  margin: 2px 4px;"
    "  background: @bg_base;"
    "  border-radius: 14px;"
    "}"

    "/* Window title not a pill */"
    "#window {"
    "  margin: 2px 6px;"
    "  padding: 0 4px;"
    "  color: @accent_blue;"
    "  font-weight: 400;"
    "  background: transparent;"
    "  border-radius: 0;"
    "}"

    "#clock:hover, #mpris:hover, #desk:hover, system:hover, #system:hover {"
    "  background: @bg_surface;"
    "}"

    "/* === DESK GROUP (NixOS logo + workspaces) === */"
    "#desk {"
    "  padding: 4px 16px;"
    "}"

    "/* Flatten inner modules inside desk */"
    "#desk #custom-nixos,"
    "#desk #workspaces {"
    "  background: transparent;"
    "  margin: 0;"
    "  padding: 0;"
    "}"

    "/* NixOS logo inside desk */"
    "#desk #custom-nixos {"
    "  padding-right: 10px;"
    "  font-weight: 600;"
    "  color: @accent_blue;"
    "}"

    "/* Workspaces container (flattened) */"
    "#desk #workspaces {"
    "  padding: 0;"
    "  margin: 0;"
    "}"

    "/* Workspace buttons (single pill style) */"
    "#desk #workspaces button {"
    "  background: transparent;"
    "  margin: 0 3px;"
    "  padding: 2px 8px;"
    "  min-height: 24px;"
    "  border-radius: 6px;"
    "  border: 1px solid transparent;"
    "  transition: background 100ms;"
    "}"
    
    "#desk #workspaces button:hover {"
    "  background: rgba(255,255,255,0.07);"
    "}"
    "#desk #workspaces button.focused, #desk #workspaces button.active {"
    "  background: rgba(230,195,132,0.18);"
    "  color: @accent_yellow;"
    "  font-weight: 600;"
    "  border: 1px solid @accent_yellow;"
    "}"
    "#desk #workspaces button.urgent {"
    "  background: @accent_red;"
    "  color: #1f1f28;"
    "  font-weight: 600;"
    "  border: 1px solid @accent_red;"
    "}"

    "/* MPRIS states */"
    "#mpris.playing { background: rgba(125,108,200,0.30); }"
    "#mpris.paused  { background: rgba(125,108,200,0.15); font-style: italic; }"

    "/* Base state colouring */"
    "#network.disconnected { color: @accent_red; font-weight: 600; }"
    "#cpu.warning  { color: @accent_orange; }"
    "#cpu.critical { color: @accent_red; font-weight: 600; }"
    "#battery.warning  { color: @accent_orange; }"
    "#battery.critical { color: @accent_red; font-weight: 600; }"
    "#battery.charging { color: @accent_green; font-weight: 500; }"
    "#wireplumber.muted { color: @accent_magenta; }"

    "tooltip {"
    "  border-radius: 10px;"
    "  background: rgba(40,40,52,0.95);"
    "  border: 1px solid rgba(255,255,255,0.08);"
    "  padding: 6px 10px;"
    "}"
    "tooltip label { color: @fg; }"

    "/* === SYSTEM GROUP PILL === */"
    "system, #system {"
    "  padding: 6px 16px;"
    "}"

    "system #network, #system #network,"
    "system #cpu, #system #cpu,"
    "system #memory, #system #memory,"
    "system #wireplumber, #system #wireplumber,"
    "system #battery, #system #battery {"
    "  background: transparent;"
    "  margin: 0;"
    "  padding: 2px 8px;"
    "  border-radius: 0;"
    "  color: inherit;"
    "}"

    "system #network, #system #network { padding-left: 4px; }"
    "system #battery, #system #battery { padding-right: 12px; }"

    "system #cpu, #system #cpu,"
    "system #memory, #system #memory,"
    "system #wireplumber, #system #wireplumber,"
    "system #battery, #system #battery {"
    "  border-left: 1px solid rgba(255,255,255,0.07);"
    "}"

    "system #cpu.warning, #system #cpu.warning { color: @accent_orange; }"
    "system #cpu.critical, #system #cpu.critical { color: @accent_red; font-weight: 600; }"
    "system #battery.warning, #system #battery.warning { color: @accent_orange; }"
    "system #battery.critical, #system #battery.critical { color: @accent_red; font-weight: 600; }"
    "system #battery.charging, #system #battery.charging { color: @accent_green; }"
    "system #wireplumber.muted, #system #wireplumber.muted { color: @accent_magenta; }"

    ":focus { outline: none; }"

    "/* DEBUG (uncomment to visualize container bounds) */"
    "/* #desk { outline: 1px dashed #4fa; } */"
    "/* system { outline: 1px dashed #fa4; } */"
  ];
}
