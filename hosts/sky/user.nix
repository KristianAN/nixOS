{
  users.users = {
    kristian = {
      isNormalUser = true;
      openssh.authorizedKeys.keys = [
        # TODO: Add your SSH public key(s) here, if you plan on using SSH to connect
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQC01cvaT1DaPu/3BtuMypNkKH7jflDZ/LKex6/QM3HZaZS1wlvSvoK3yEYAwRZpHCLzGpJ7dk0+bj/UzKsgyatGGeVzGTp7meJLjHgMYJxPw3+Xd25HvaL+7Elhej0x8YOO/QkvgJW/VN3KVhM1Nbd4MfF0dXLi6zHmBHtjLH/haUpb0ueY/4Aa6lNzdR8GNQ6EmKvSDC98KNw9CTrQ7pjrCEJwgGq6GwomXPJ4IhMwRKElm2f8HN3aT+/aVuQ4p9BMyP10emuxSxAPmPy/Ff6ShlnrV7wXJl4RFC9FQa/K2/SUY/pegs9PBlgEMeyfYeZPlYggnCxlAgfI9hAGSvmfWjcwFfbQ6rbpvUZnE2A8VdkpVRx6gelsiH4z8lHUCjoAUdFPcHpQAoK1uc4QSN5s1cOtSM0Yhr23WFKaZvE7yF/0OPCY3KEGsvzCU6Z8IL2iEV76XL9mZIUAzio++ZUCHHJMgKYdD/0n+jJb1t8CbiBLDrmqwXHArcD2dp8Srlc= kristian@rubble"
      ];
      extraGroups = ["wheel" "networkmanager" "docker" "libvirtd" "video" "audio" "plugdev"];
    };
  };
}
