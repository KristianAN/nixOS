{ ... }:
{
  imports = [
    ../common
  ];

  wayland.windowManager.sway.extraOptions = [
    "--unsupported-gpu"
  ];

}
