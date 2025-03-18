{
  pkgs,
  lib,
  ...
}:
{

  # Include all three caches in substituters
  nix.settings.substituters = lib.mkAfter [
    "https://kristian.cachix.org"
    "https://nix-community.cachix.org"
  ];

  # Include all three trusted public keys
  nix.settings.trusted-public-keys = lib.mkAfter [
    "kristian.cachix.org-1:I9DC4coD583Us0HFeCW6KiiXbk6uhlWfB2MPxjJiiUI="
    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
  ];

  environment.systemPackages = [ pkgs.cachix ];
}
