{ lib, pkgs, ... }:

let
  kanagawa = import ./kanagawa-theme.nix;
in
{
  services.mako = {
    enable = true;
    package = pkgs.mako;
  };

    xdg.configFile."mako/config".text = lib.concatStringsSep "\n" [
      "font=${kanagawa.font} ${(builtins.toString (kanagawa.fontSize - 4))}"
      "default-timeout=5000"
      "margin=12"
      "width=360"
      "anchor=top-right"
      "layer=top"
      "max-visible=3"
      "border-size=1"
      ""
      "background-color=${kanagawa.background}"
      "text-color=${kanagawa.foreground}"
      "border-color=${kanagawa.border}"
      ""
      "[urgency=low]"
      "background-color=${kanagawa.background}"
      "text-color=${kanagawa.muted}"
      "border-color=${kanagawa.border}"
      ""
      "[urgency=normal]"
      "background-color=${kanagawa.background}"
      "text-color=${kanagawa.foreground}"
      "border-color=${kanagawa.accentYellow}"
      ""
      "[urgency=critical]"
      "background-color=${kanagawa.accentRed}"
      "text-color=${kanagawa.background}"
      "border-color=${kanagawa.accentRed}"
    ];

}
