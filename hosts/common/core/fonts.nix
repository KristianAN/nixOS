{ pkgs, ... }: {
  fonts.packages = with pkgs; [
    (nerdfonts.override {
      fonts = [
        "FiraCode"
        "Hack"
        "Iosevka"
        "JetBrainsMono"
        "DejaVuSansMono"
        "UbuntuMono"
        "SourceCodePro"
        "FiraMono"
        "LiberationMono"
        "Noto"
        "RobotoMono"
        "Monaspace"
      ];
    })
    noto-fonts
    noto-fonts-emoji
    liberation_ttf
    fira-code
    fira-code-symbols
    material-design-icons
  ];
}
