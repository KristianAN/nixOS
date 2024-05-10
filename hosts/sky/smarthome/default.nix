{ config, pkgs, ... }: {
  imports = [
    ./zigbee2mqtt.nix
    ./wireguard.nix
  ];
}
