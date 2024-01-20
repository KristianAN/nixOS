{ pkgs, ... }:
{
  programs.git = {
    enable = true;
    package = pkgs.gitAndTools.gitFull;
    userEmail = "kristian@krined.no";
    userName = "Kristian Nedrevold-Hansen";
    extraConfig = {
      core.editor = "nvim";
      github.user = "KristianAN";
    };

    ignores = [
      "*~"
      "\#*\#"
      "/.emacs.desktop"
      "/.emacs.desktop.lock"
      "*.elc"
      "auto-save-list"
      "tramp"
      ".\#*"
      ".org-id-locations"
      "*_archive"
      "*_flymake.*"
      "*.rel"
      "flycheck_*.el"
      ".projectile"
      ".dir-locals.el"
    ];
  };
}
