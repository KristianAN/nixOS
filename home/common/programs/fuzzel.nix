{ pkgs, ... }:
{
  programs.fuzzel = {
    enable = true;
    settings = {
      main = {
        terminal = "${pkgs.wezterm}/bin/wezterm";
        layer = "overlay";
        width = 45;
        lines = 5;
      };
      colors = {
        background = "31363bff";
        text = "eff0f1ff";
        match = "ffffffff";
        selection-match = "ffffffff";
        selection = "3daee9ff";
        selection-text = "eff0f1ff";
        border = "232629ff";
      };
    };
  };
}
