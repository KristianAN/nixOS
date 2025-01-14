{ ... }:
{
  programs.foot = {
    enable = true;

    settings = {
      main = {
        term = "xterm-256color";
        font = "Iosevka Nerd Font:size=13";
        dpi-aware = "no";
      };

      mouse = {
        hide-when-typing = "yes";
      };

      cursor = {
        blink = "yes";
      };

      colors = {
        alpha = "1";
        foreground = "eef1f8";
        background = "14161b";

        selection-foreground = "e0e2ea";
        selection-background = "9b9ea4";

        regular0 = "07080d";
        regular1 = "ffc0b9";
        regular2 = "b3f6c0";
        regular3 = "c0a36e";
        regular4 = "fce094";
        regular5 = "ffcaff";
        regular6 = "6a9589";
        regular7 = "005523";

        bright0 = "6b5300";
        bright1 = "e82424";
        bright2 = "590008";
        bright3 = "007373";
        bright4 = "a6dbff";
        bright5 = "470045";
        bright6 = "2e5942";
        bright7 = "d9cf77";

        "16" = "ffa066";
        "17" = "ff5d62";
      };
    };
  };
}
