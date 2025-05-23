{ pkgs, ... }:
{
  config = {
    users = {
      extraUsers."openhab" = {
        isSystemUser = true;
        isNormalUser = false;
        group = "openhab";
        uid = 1200;
        home = "/opt/openhab";
        createHome = true;
        shell = pkgs.shadow;
      };
      extraGroups."openhab".gid = 1200;
    };

    virtualisation.oci-containers.containers = {
      openhab = {
        image = "openhab/openhab:latest";
        # args = ;
        # entrypoint = ;
        volumes = [
          "/opt/openhab/conf:/openhab/conf"
          "/opt/openhab/userdata:/openhab/userdata"
          "/opt/openhab/addons:/openhab/addons"
          "/etc/localtime:/etc/localtime:ro"
          # "/etc/timezone:/etc/timezone:ro"
        ];
        extraOptions = [
          "--net=host"
          "-e"
          "USER_ID=1200"
          "-e"
          "GROUP_ID=1200"
          "-e"
          "OPENHAB_HTTP_PORT=8080"
          "-e"
          "OPENHAB_HTTPS_PORT=8443"
          "--memory=2g"
        ];
      };
    };
  };
}
