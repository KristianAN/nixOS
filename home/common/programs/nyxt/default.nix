{ pkgs, ... }:
{
  home.packages = [
    pkgs.nyxt
    gst_all_1.gst-libav
    gst_all_1.gst-plugins-bad
    gst_all_1.gst-plugins-base
    gst_all_1.gst-plugins-good
    gst_all_1.gst-plugins-ugly
  ];

  xdg.configFile."nyxt/default.lisp".source = ./default.lisp;
}
