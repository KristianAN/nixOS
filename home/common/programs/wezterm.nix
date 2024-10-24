{ ... }:
{
  programs.wezterm.enable = true;
  programs.wezterm.enableZshIntegration = true;
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
      foreground = "#c5c9c5",
      background = "#181616",

      cursor_bg = "#C8C093",
      cursor_fg = "#C8C093",
      cursor_border = "#C8C093",

      selection_fg = "#C8C093",
      selection_bg = "#2D4F67",

      scrollbar_thumb = "#16161D",
      split = "#16161D",

      ansi = {
        "#0D0C0C",
        "#C4746E",
        "#8A9A7B",
        "#C4B28A",
        "#8BA4B0",
        "#A292A3",
        "#8EA4A2",
        "#C8C093",
      },
      brights = {
        "#A6A69C",
        "#E46876",
        "#87A987",
        "#E6C384",
        "#7FB4CA",
        "#938AA9",
        "#7AA89F",
        "#C5C9C5",
      },
      indexed = { [16] = "#B6927B", [17] = "#B98D7B" },
    }

    config.window_padding = {
      left = 0,
      right = 0,
      top = 0,
      bottom = 0,
    }

    config.enable_tab_bar = false

    config.window_background_opacity = 0.95

    return config
  '';
}
