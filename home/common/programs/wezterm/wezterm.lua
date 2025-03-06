local wezterm = require 'wezterm'

local config = {
}

if wezterm.config_builder then
  config = wezterm.config_builder()
end

config.force_reverse_video_cursor = true

config.color_scheme = "NvimDark"

config.font_size = 13

config.use_fancy_tab_bar = true

config.enable_tab_bar = true

config.hide_tab_bar_if_only_one_tab = true

config.window_padding = {
  left = 0,
  right = 0,
  top = 0,
  bottom = 0,
}

config.window_background_opacity = 1

config.leader = { key = 'u', mods = 'CTRL', timeout_milliseconds = 1000 }

config.keys = {
  -- splitting
  { mods = "LEADER", key = "-", action = wezterm.action.SplitVertical { domain = 'CurrentPaneDomain' } },
  { mods = "LEADER", key = "|", action = wezterm.action.SplitHorizontal { domain = 'CurrentPaneDomain' }, },
  -- Tab Navigation
  { mods = "LEADER", key = "1", action = wezterm.action { ActivateTab = 0 }, },
  { mods = "LEADER", key = "2", action = wezterm.action { ActivateTab = 1 }, },
  { mods = "LEADER", key = "3", action = wezterm.action { ActivateTab = 2 }, },
  { mods = "LEADER", key = "4", action = wezterm.action { ActivateTab = 3 }, },
  { mods = "LEADER", key = "5", action = wezterm.action { ActivateTab = 4 }, },
  { mods = "LEADER", key = "6", action = wezterm.action { ActivateTab = 5 }, },
  { mods = "LEADER", key = "7", action = wezterm.action { ActivateTab = 6 }, },
  { mods = "LEADER", key = "8", action = wezterm.action { ActivateTab = 7 }, },
  { mods = "LEADER", key = "9", action = wezterm.action { ActivateTab = 8 }, },
  -- Pane Navigation
  { mods = "LEADER", key = "h", action = wezterm.action { ActivatePaneDirection = "Left" } },
  { mods = "LEADER", key = "l", action = wezterm.action { ActivatePaneDirection = "Right" } },
  { mods = "LEADER", key = "k", action = wezterm.action { ActivatePaneDirection = "Up" } },
  { mods = "LEADER", key = "j", action = wezterm.action { ActivatePaneDirection = "Down" } },
  { mods = "LEADER", key = "x", action = wezterm.action.CloseCurrentPane { confirm = false } },
}
return config
