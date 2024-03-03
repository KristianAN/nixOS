{ config, pkgs, lib, ... }:
let
  # read from json
  config = builtins.fromJSON (builtins.readFile /home/kristian/.configuration/mqtt.json);
in
{
  networking.firewall.allowedTCPPorts = [ 8081 1883 ];

  services.mosquitto = {
    enable = true;
    package = pkgs.mosquitto;
    listeners = [
      {
        acl = [ "readwrite zigbee2mqtt/#" ];
        users = {
          zigbee2mqtt = {
            password = config.mqtt_password;
          };
        };
        port = 1883;
      }
    ];
  };

  services.zigbee2mqtt = {
    enable = true;
    package = pkgs.zigbee2mqtt;
    settings = {

      permit_join = false;
      serial.port = "/dev/zigbee";
      frontend.port = 8081;

      device_options = {
        retain = true;
      };
      advanced = {
        channel = 25;
        network_key = "GENERATE";
        log_output = [ "console" ];
        log_level = "warn";
      };
      mqtt = {
        version = 5;
        server = "mqtt://localhost:1883";
        user = "zigbee2mqtt";
        password = config.mqtt_password;
      };
    };
  };
}

