{
  pkgs,
  lib,
  ...
}:
let
  # read from json
  mqttConfig = {
    network_key = "temp";
    mqtt_password =  "superduperpass";
  };
in
{
  networking.firewall = {
    enable = true;
    allowedTCPPorts = [
      8081
      8080
      8443
      1883
      8581
    ];
  };
  services.udev.extraRules = ''
    # ConBee II
    SUBSYSTEM=="tty", ATTRS{idVendor}=="1cf1", ATTRS{idProduct}=="0030", SYMLINK+="ttyACM-conbee", MODE="0666"
  '';
  services.mosquitto = {
    enable = true;
    listeners = [
      {
        address = "0.0.0.0";  
        port = 1883;
        users = {
          zigbee2mqtt = {
            acl = [ "readwrite #" ];
            password = mqttConfig.mqtt_password;
          };
        };
      }
    ];
  };

  services.homebridge = {
    enable = true;
    openFirewall = true;
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
        host = "192.168.4.100";
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
        server = "mqtt://192.168.4.100:1883";
        user = "zigbee2mqtt";
        password = mqttConfig.mqtt_password;
      };
    };
  };
}
