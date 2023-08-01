{ pkgs }:

with pkgs;
let
  modifier = "Mod4";
  workspaceScript = pkgs.writeShellScript "wrk.sh" ''
  "kitty tmux new-session -A";
    '';
in
{
  "${modifier}+shift+t" = "exec ${workspaceScript}";
}
