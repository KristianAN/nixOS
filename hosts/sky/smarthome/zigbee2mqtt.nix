{ config, pkgs, lib, ... }:
let 
  # read from json
<<<<<<< HEAD
  config = lib.builtins.fromJSON (builtins.readFile /home/kristian/.configuration/mqtt.json);

in 
  networking.firewall.allowedTCPPorts = [ 8080 ];

  services.zigbee2mqtt = {
    enable = true;
    package = pkgs.zigbee2mqtt;
    settings = {

=======
  config = lib.builtins.fromJSON (builtins.readFile ~/.configuration/mqtt.json);
in 
  networking.firewall.allowedTCPPorts = [ 8080 ];

  services.zigbee2mqtt = {
    enable = true;
    package = pkgs.zigbee2mqtt;
    settings = {

>>>>>>> 553f8a0fdfb3a1a806495ba296ed7c491c7ea06c
      permit_join = false;
      # serial.port = "/dev/ttyUSB0";
      frontend = true;
      device_options = {
        retain = true;
      };
      advanced = {
        channel = 25;
        network_key = config.network_key;
        log_output = [ "console" ];
        log_level = "warn";
      };
      mqtt = {
        version = 5;
        server = "mqtt://localhost:1883";
        user = "zigbee2mqtt";
        password =  config.mqtt_password;
      };
    };
  };
