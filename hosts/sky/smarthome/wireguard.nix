{ config, pkgs, ... }:
{
  systemd.tmpfiles.rules = [
    "d /configs/wireguard 0755 kristian kristian-" #The - disables automatic cleanup, so the file wont be removed after a period
  ];

  virtualisation.oci-containers.containers = {
    wireguard = {
      autoStart = true;
      image = "lscr.io/linuxserver/wireguard:latest";
      environment = {
        TZ = "Europe/Oslo";
        PUID = "1000";
        PGID = "1000";
        PEERS = "1";
        PEERDNS = "auto";
        ALLOWEDIPS = "0.0.0.0/0";
        SERVERPORT = "51820";
      };
      ports = [ "51820:51820/udp" ];
      volumes = [
        "/configs/wireguard:/config"
      ];
      extraOptions = [
        "--cap-add=NET_ADMIN,NET_RAW"
        "--cap-add=SYS_MODULE"
        ''--sysctl="net.ipv4.conf.all.src_valid_mark=1"''
      ];
    };
  };
}
