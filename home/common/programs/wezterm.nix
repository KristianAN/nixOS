{ ... }:
{
  programs.wezterm.enable = true;
  programs.wezterm.extraConfig = ''
    local wezterm = require 'wezterm'

    local config = {
    }

    if wezterm.config_builder then
      config = wezterm.config_builder()
    end

    config.xcursor_theme="Kanagawa-BL-Cursors"

    force_reverse_video_cursor = true

    config.colors = {
      foreground = "#dcd7ba",
      background = "#1f1f28",

      cursor_bg = "#c8c093",
      cursor_fg = "#c8c093",
      cursor_border = "#c8c093",

      selection_fg = "#c8c093",
      selection_bg = "#2d4f67",

      scrollbar_thumb = "#16161d",
      split = "#16161d",

      ansi = { "#090618", "#c34043", "#76946a", "#c0a36e", "#7e9cd8", "#957fb8", "#6a9589", "#c8c093" },
      brights = { "#727169", "#e82424", "#98bb6c", "#e6c384", "#7fb4ca", "#938aa9", "#7aa89f", "#dcd7ba" },
      indexed = { [16] = "#ffa066", [17] = "#ff5d62" },
    }

    config.window_padding = {
      left = 0,
      right = 0,
      top = 0,
      bottom = 0,
    }

    config.enable_tab_bar = false

    config.window_background_opacity = 1

    config.leader = { key = 'u', mods = 'CTRL', timeout_milliseconds = 1000 }

    config.keys = {
      -- splitting
      {
        mods   = "LEADER",
        key    = "-",
        action = wezterm.action.SplitVertical { domain = 'CurrentPaneDomain' }
      },
      {
        mods   = "LEADER",
        key    = "\",
        action = wezterm.action.SplitHorizontal { domain = 'CurrentPaneDomain' }
      }
    }
    return config
  '';
}
