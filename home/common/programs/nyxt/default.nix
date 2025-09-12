{ pkgs, ... }:
{
  home.packages = [
    pkgs.nyxt
    pkgs.gst_all_1.gst-libav
    pkgs.gst_all_1.gst-plugins-bad
    pkgs.gst_all_1.gst-plugins-base
    pkgs.gst_all_1.gst-plugins-good
    pkgs.gst_all_1.gst-plugins-ugly
  ];

  xdg.configFile."nyxt/config.lisp".source = ./config.lisp;
}
