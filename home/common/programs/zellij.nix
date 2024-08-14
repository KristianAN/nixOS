{ ... }:
{
  programs.zellij = {
    enable = true;
  };

  # NOTE: the module only supports YAML config which is deprecated
  home.file.zellij = {
    target = ".config/zellij/config.kdl";
    text = ''
      simplified_ui true
      pane_frames false
      default_layout "compact"
      keybinds {
        normal clear-defaults=true {
          bind "Ctrl u" { SwitchToMode "tmux"; }
        }
        tmux clear-defaults=true {
          bind "Esc" { SwitchToMode "Normal"; }

          bind "Ctrl e" { WriteChars "nvim"; Write 13; SwitchToMode "Normal"; }

          bind "Ctrl u" { CloseFocus; SwitchToMode "Normal"; }
          bind "z" { ToggleFocusFullscreen; SwitchToMode "Normal"; }
          bind "d" { Detach; }
          bind "s" { ToggleActiveSyncTab; SwitchToMode "Normal"; }

          bind "h" { MoveFocus "Left"; SwitchToMode "Normal"; }
          bind "l" { MoveFocus "Right"; SwitchToMode "Normal"; }
          bind "j" { MoveFocus "Down"; SwitchToMode "Normal"; }
          bind "k" { MoveFocus "Up"; SwitchToMode "Normal"; }

          bind "g" { SwitchToMode "Locked"; }
          bind "p" { SwitchToMode "Pane"; }
          bind "t" { SwitchToMode "Tab"; }
          bind "n" { SwitchToMode "Resize"; }
          bind "o" { SwitchToMode "Session"; }
          bind "q" { Quit; }
          bind "r" { SwitchToMode "RenameTab"; TabNameInput 0; }
          bind "f" { ToggleFloatingPanes; SwitchToMode "Normal"; }

          bind "-" { NewPane "Down"; SwitchToMode "Normal"; }
          bind "|" { NewPane "Right"; SwitchToMode "Normal"; }

          bind "1" { GoToTab 1; SwitchToMode "Normal"; }
          bind "2" { GoToTab 2; SwitchToMode "Normal"; }
          bind "3" { GoToTab 3; SwitchToMode "Normal"; }
          bind "4" { GoToTab 4; SwitchToMode "Normal"; }
          bind "5" { GoToTab 5; SwitchToMode "Normal"; }
          bind "6" { GoToTab 6; SwitchToMode "Normal"; }
          bind "7" { GoToTab 7; SwitchToMode "Normal"; }
          bind "8" { GoToTab 8; SwitchToMode "Normal"; }
          bind "9" { GoToTab 9; SwitchToMode "Normal"; }

          bind "c" { NewTab; SwitchToMode "Normal"; }

          bind "Ctrl l" { GoToNextTab; SwitchToMode "Normal"; }
          bind "Ctrl h" { GoToPreviousTab; SwitchToMode "Normal"; }
        }
      }

      theme "kanagawa"
      themes {
        kanagawa{
          bg "#DCD7BA"
          fg "#1F1F28"
          white "#090618"
          black "#dcd6ba"
          red "#e82424"
          green "#98bb6c"
          yellow "#e6c384"
          blue "#7fb4ca"
          magenta "#938aa9"
          cyan "#7aa89f"
          orange "#FFA066"
        }
      }
    '';
  };
}
