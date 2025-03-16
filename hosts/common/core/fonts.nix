{ pkgs, ... }:
{
  fonts.packages = [ pkgs.aporetic ] ++ builtins.filter pkgs.lib.attrsets.isDerivation (builtins.attrValues pkgs.nerd-fonts);
}
