{ pkgs, ... }:
let
  fenv = {
    inherit (pkgs.fishPlugins.foreign-env) src;
    name = "foreign-env";
  };
in
{
  programs.fish = {
    enable = true;
    generateCompletions = true;
    plugins = [ fenv ];
  };

  programs.zoxide = {
    enable = true;
    enagleFishIntegration = true;
  };
}
