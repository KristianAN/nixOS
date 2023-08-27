{ pkgs, ... }: {
  home = {
    packages = with pkgs; [
      emacs-all-the-icons-fonts
      fd
      gcc
      ripgrep
      coursier
    ];
  };
  programs.emacs = {
    enable = true;
    package = pkgs.emacs29-pgtk;
  };

  home.sessionVariables.DOOMDIR = "~/nix/nixOS/home/config/doom";
}
