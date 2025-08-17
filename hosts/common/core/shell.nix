{
  ...
}:
let
  fzfConfig = ''
    set -x FZF_DEFAULT_OPTS "--preview='bat {} --color=always'" \n
    set -x SKIM_DEFAULT_COMMAND "rg --files || fd || find ."
  '';

  themeConfig = ''
    set -g theme_display_date no
    set -g theme_display_git_master_branch no
    set -g theme_nerd_fonts yes
    set -g theme_newline_cursor yes
    set -g theme_color_scheme solarized
  '';

  useStarshipOrDumb = ''
    if test "$TERM" = "dumb"
        set -e fish_key_bindings
        set -g fish_prompt '> '
        set -g HISTFILE ~/.tramp-histfile
        functions -e fish_prompt
        function fish_prompt
            echo '> '
        end
    else
        starship init fish | source
    end
  '';

  fishConfig =
    ''
      set fish_greeting
      fish_vi_key_bindings
    ''
    + fzfConfig
    + themeConfig
    + useStarshipOrDumb;

in
{
  programs.fish = {
    enable = true;
    vendor = {
      completions.enable = true;
      config.enable = true;
      functions.enable = true;
    };
    # Remove the interactiveShellInit that overrides the settings in fishConfig
    interactiveShellInit = ''
      eval (direnv hook fish)
      any-nix-shell fish --info-right | source
    '';

    shellAliases = {
      eat = "emacsclient -c -e \"(eat)\""
    };
    
    shellInit = fishConfig;
  };
}
