{
  pkgs,
  lib,
  ...
}:
{

  nix.settings.substituters = lib.mkAfter [
    "https://cache.iog.io"
    "https://kristian.cachix.org"
    "https://nix-community.cachix.org"
  ];

  # Include all three trusted public keys
  nix.settings.trusted-public-keys = lib.mkAfter [
    "kristian.cachix.org-1:I9DC4coD583Us0HFeCW6KiiXbk6uhlWfB2MPxjJiiUI="
    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
  ];

  environment.systemPackages = [ pkgs.cachix ];
}
