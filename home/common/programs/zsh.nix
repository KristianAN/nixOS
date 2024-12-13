{ ... }:
{
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    autocd = true;
    historySubstringSearch.enable = true;
    shellAliases = {
      za = "zellij a";
      zn = "zellij -s";
      zqa = "zellij kill-all-sessions -y && zellij delete-all-sessions -y";
    };
    syntaxHighlighting.enable = true;
    initExtra = ''
      [ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
        source "$EAT_SHELL_INTEGRATION_DIR/zsh"
    '';
  };

  programs.zoxide = {
    enable = true;
    enableZshIntegration = true;
  };

}
