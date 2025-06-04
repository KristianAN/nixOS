{ pkgs, ... }:
{
  programs.git = {
    enable = true;
    package = pkgs.gitAndTools.gitFull;
    userEmail = "kristiananh@proton.me";
    userName = "Kristian Nedrevold-Hansen";
    extraConfig = {
      core.editor = "emacsclient";
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
