{ lib, ... }:
{
  networking.hostName = "sky";
  networking.networkmanager.enable = true;
  networking.interfaces.eno1.ipv4.addresses = [
    {
      address = "192.168.4.100";
      prefixLength = 24;
    }
  ];
  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.enp2s0f0u3.useDHCP = lib.mkDefault true;
  # networking.interfaces.enp4s0.useDHCP = lib.mkDefault true;
}
