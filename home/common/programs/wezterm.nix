{ ... }:
{
  programs.wezterm.enable = true;
  programs.wezterm.enableZshIntegration = true;
  programs.wezterm.extraConfig = ''
    local wezterm = require 'wezterm'
    
    local config = {}
    
    if wezterm.config_builder then
      config = wezterm.config_builder()
    end
    
    config.colors = {
      -- WezTerm uses these colors for text rendering
      foreground = "#b5bdc5",
      background = "#080c10",
      cursor_bg = "#b5bdc5", -- Set this to the color you want for the cursor
      cursor_border = "#b5bdc5",
      cursor_fg = "#080c10", -- And this to the color you want for the cursor text
      selection_bg = "#012749",
      selection_fg = "#b5bdc5",

      -- The color of the scrollbar "thumb"; the portion that represents the current viewport
      scrollbar_thumb = "#393939",

      -- The color of the split lines between panes
      split = "#393939",

      -- ansi and bright color definitions
      ansi = {"#262626", "#fa4d56", "#42be65", "#d2a106", "#5080ff", "#a665d0", "#0ab6ba", "#e0e0e0"},
      brights = {"#474747", "#ff8389", "#74e792", "#f8e6a0", "#78a9ff", "#be95ff", "#57e5e5", "#cac5c4"},
    }

    config.window_padding = {
      left = 0,
      right = 0,
      top = 0,
      bottom = 0,
    }

    config.enable_tab_bar = false

    return config
  '';
}
