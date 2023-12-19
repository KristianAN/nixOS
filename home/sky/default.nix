{pkgs, ...}: {
  imports = [
    ../common
  ];

  home.packages = with pkgs; [
    defined in ../common/default.nix
    zigbee2mqtt 
  ];
  
}
