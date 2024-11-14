{
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.blacklistedKernelModules = [ "elan_i2c" ];
  boot.supportedFilesystems = [ "ntfs" ];
}
