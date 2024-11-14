{
  ...
}:
{
  time.timeZone = "Europe/Oslo";
  i18n.supportedLocales = [
    "en_US.UTF-8/UTF-8"
  ];

  # This setups a SSH server. Very important if you're setting up a headless system.
  # Feel free to remove if you don't need it.

  security.rtkit.enable = true;

  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;
  };

  services.openssh = {
    enable = true;
    # Forbid root login through SSH.
    settings = {
      PermitRootLogin = "no";
      PasswordAuthentication = false;
    };
  };

  services.blueman.enable = true;

}
