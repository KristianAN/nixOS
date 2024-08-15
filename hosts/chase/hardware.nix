{
  config,
  lib,
  pkgs,
  ...
}: {
  hardware.opengl.enable = true;
  hardware.logitech.wireless.enable = true;
  hardware.opengl.extraPackages = with pkgs; [
    vaapiIntel
    libvdpau-va-gl
    intel-media-driver
  ];

  hardware.bluetooth.enable = true;
  hardware.keyboard.zsa.enable = true;
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  # high-resolution display
  # hardware.video.hidpi.enable = lib.mkDefault true;
  # Set your system kind (needed for flakes)
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

  boot.initrd.availableKernelModules = [ "xhci_pci" "thunderbolt" "vmd" "nvme" "usbhid" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "i915" "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/ec5e593e-551a-4aae-a25b-c59e111d92e8";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/2234-821A";
      fsType = "vfat";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/1e472841-888d-4ab3-b85f-80cddff26abe"; }
    ];
  
  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.wlp1s0.useDHCP = lib.mkDefault true;

}
