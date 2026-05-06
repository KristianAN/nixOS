{ pkgs, ... }:
{
home.packages = [
  pkgs.adwaita-icon-theme
  pkgs.hicolor-icon-theme
];
  
  gtk = {
    enable = true;
    iconTheme = {
      package = pkgs.kdePackages.breeze-icons;
      name = "breeze-dark";
    };
    theme = {
      package = pkgs.kanagawa-gtk-theme;
      name = "Kanagawa-BL";
    };
    gtk4.extraConfig = {
      gtk-application-prefer-dark-theme = true;
    };
  };

}
