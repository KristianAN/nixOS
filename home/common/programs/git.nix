{ pkgs, ... }:
{
  programs.git = {
    enable = true;
    settings = {
      user.email = "kristiananh@proton.me";
      user.name = "Kristian Nedrevold-Hansen";
      core.editor = "emacsclient";
      github.user = "KristianAN";
      rebase = {
        updateRefs = true;
      };
      merge = {
      conflictStyle = "diff3";
        tool = "ediff";
        keepBackup = false;
        trustExitCode = true;
        ediff.keepBackup = false;
        ediff.cmd = ''
          emacsclient --eval \"\
          (progn\
            (defun ediff-write-merge-buffer ()\
              (let ((file ediff-merge-store-file))\
                (set-buffer ediff-buffer-C)\
                (write-region (point-min) (point-max) file)\
                (message \\\"Merge buffer saved in: %s\\\" file)\
                (set-buffer-modified-p nil)\
                (sit-for 1)))\
            (setq ediff-quit-hook 'kill-emacs\
                  ediff-quit-merge-hook 'ediff-write-merge-buffer)\
            (ediff-merge-files-with-ancestor \\\"$LOCAL\\\" \\\"$REMOTE\\\"\
                                             \\\"$BASE\\\" nil \\\"$MERGED\\\"))\"
        '';
      };
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
