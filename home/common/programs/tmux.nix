{ pkgs, ... }:
{
  programs.tmux = {
    enable = true;
    shell = "${pkgs.bash}/bin/bash";
    escapeTime = 0;
    prefix = "C-a"; # set prefix to ctrl + a
    clock24 = true; # 24 hour clock
    historyLimit = 100000; # increase history limit
    baseIndex = 1; # start window/pane index at 1
    sensibleOnTop = true; # use tmux-sensible
    disableConfirmationPrompt = true; # i know what i'm doing, kill without prompt
    tmuxinator.enable = true;
    plugins = with pkgs.tmuxPlugins; [
      resurrect
      continuum
      vim-tmux-navigator
      better-mouse-mode
    ];
    extraConfig = ''
      set -g default-terminal "screen-256color"
      set -as terminal-features ",xterm-256color:RGB"
      set -g set-titles on
      set -g set-titles-string "#W #{command} #T #{session_path}"
      set -g focus-events on
      set -g set-clipboard on
      
      # enable using a mouse
      set -g mouse on

      # automatically renumber windows after one is closed
      set-option -g renumber-windows on

      # split panes with | and -
      bind | split-window -h -c "#{pane_current_path}"
      bind - split-window -v -c "#{pane_current_path}"
      unbind '"'
      unbind %

      set-option -g status-position bottom
      set-option -g status on
      set-option -g status-interval 1
      set-option -g automatic-rename on
      set-option -g automatic-rename-format '#{b:pane_current_path}'

      # tmuxline
      set -g status "on"
      set -g status-justify "left"
      set -g status-style "none,bg=default"
      set -g status-left-style "none"
      set -g status-left-length "100"
      set -g status-right-style "none"
      set -g status-right-length "100"
      set -g pane-border-style "fg=#2e3440,bg=default"
      set -g pane-active-border-style "fg=#3b4252,bg=default"
      set -g pane-border-status bottom
      set -g pane-border-format ""
      set -g message-style "fg=brightwhite,bg=default"
      set -g message-command-style "fg=brightwhite,bg=default"
      setw -g window-status-activity-style "none"
      setw -g window-status-separator ""
      setw -g window-status-style "none,fg=brightwhite,bg=default"
      set -g status-left "#[fg=#8fbcbb,bg=default,bold]#S #[fg=brightwhite,bg=default,nobold,nounderscore,noitalics]"
      set -g status-right "#[fg=#616E88,bg=default]%k:%M #[fg=#616E88,bg=default] %Y-%m-%d "
      setw -g window-status-format "#[fg=#616E88,bg=default] #I#[fg=#616E88,bg=default] #W "
      setw -g window-status-current-format "#[fg=#5e81ac,bg=default] #I#[fg=brightwhite,bg=default] #W "
    '';
  };
}
