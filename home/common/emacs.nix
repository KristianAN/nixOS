{ pkgs, ... }: {
  home = {
    packages = with pkgs; [
      emacs-all-the-icons-fonts
      fd
      gcc
      ripgrep
      cmake
      gnumake
      libtool
    ];
  };
  programs.emacs = {
    enable = true;
    package = pkgs.emacs29-pgtk;
  };

  home.sessionVariables.DOOMDIR = "~/nix/nixOS/home/config/doom";
}
