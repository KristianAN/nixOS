{
  users.users = {
    kristian = {
      isNormalUser = true;
      openssh.authorizedKeys.keys = [
        # TODO: Add your SSH public key(s) here, if you plan on using SSH to connect
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCoGAVpUUHoognsrZiV0r1jRsNBRF/K0xEKjo7gam7qiahyNXyMok0BMyVifeGpkc/4LmKM14WAsk3f6NIKYy1OIPdzfi4h3M3TXEcCGqtur/8IsWoA8UWzScmdJFRCcjBfjYj08oEFN2iEgvnC2+1Z4hfQ4ZvS+uU/xi8Pp7Mi1hVAJFzxaEnBpN90z5maQfPDrc7nepxDZQ7gsFmsOF6UIBSoWI9CYgrHbRm0bnTKzoDPvK1cfdD9LNjiFsdlwQqyoTKhyyzR5h0Qdrp+zMhp4TFiamQtLeP7j/1QqWJ1I0lnHCyFB+iFyoKPzOTg1dr4U5uFQe+ss9AlSjjnRzPOIclbKd8tMFQa7ROfzSRrctS3aidUkf768MQZRUJAJyyG+LGVfbI3+okWKWF2W+bMQRGmbXEQ0Oq4uEDPNmwzY4vd3xu1sX6UWw74RKKybAq5eXhKM/VAJnv9JEJUgN1Tg+LasOUCXZE/dTlA5JnJYMuNBgKJETFU6S9c2Y8UBKc= kristian@rubble"
      ];
      extraGroups = [
        "wheel"
        "networkmanager"
        "docker"
        "libvirtd"
        "video"
        "audio"
        "plugdev"
      ];
    };
  };
}
