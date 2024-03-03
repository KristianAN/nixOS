{ config, pkgs, lib, ... }:
let
  # read from json
  config = builtins.fromJSON (builtins.readFile /home/kristian/.configuration/mqtt.json);
in
{
  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 8081 1883 ];
  };

  services.mosquitto = {
    enable = true;
    listeners = [
      {
        address = "192.168.4.198";
        port = 1883;
        users = {
          zigbee2mqtt = {
            acl = [ "pattern readwrite #" ];
            password = config.mqtt_password;
          };
        };
      }
    ];
  };

  services.zigbee2mqtt = {
    enable = true;
    package = pkgs.zigbee2mqtt;
    settings = {
      permit_join = false;
      serial = {
        port = "/dev/serial/by-id/usb-dresden_elektronik_ingenieurtechnik_GmbH_ConBee_II_DE2400945-if00";
        adapter = "deconz";
      };
      frontend = {
        port = 8081;
        host = "0.0.0.0";
      };
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
        server = "mqtt://192.168.4.198:1883";
        user = "zigbee2mqtt";
        password = config.mqtt_password;
      };
    };
  };
}

