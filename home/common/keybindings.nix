{ pkgs }:

with pkgs;
let
  modifier = "Mod4";
  workspaceScript = pkgs.writeShellScript "wrk.sh" ''
  "kitty tmux new-session -A";
    '';
  swaylockScript = pkgs.writeShellScript "swaylock.sh" ''
      swaylock \
	--screenshots \
	--clock \
	--indicator \
	--indicator-radius 100 \
	--indicator-thickness 7 \
	--effect-blur 7x5 \
	--effect-vignette 0.5:0.5 \
	--ring-color bb00cc \
	--key-hl-color 880033 \
	--line-color 00000000 \
	--inside-color 00000088 \
	--separator-color 00000000 \
	--grace 2 \
	--fade-in 0.2
  '';
in
{
  "${modifier}+shift+t" = "exec ${workspaceScript}";
  "${modifier}+alt+l" = "exec ${swaylockScript}";
}
