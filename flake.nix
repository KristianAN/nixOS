{
  description = "Kristian start with NixOS flakes";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    hell.url = "github:chrisdone/hell";
    hell.inputs.nixpkgs.follows = "nixpkgs";  
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];

      flake =
        let
          hostConfigs = {
            sky = {
              hostname = "sky";
              programs = {
                slack = false;
                discord = false;
              };
            };
            rubble = {
              hostname = "rubble";
              programs = {
                slack = false;
                discord = true;
              };
            };
            chase = {
              hostname = "chase";
              programs = {
                slack = true;
                discord = true;
              };
            };
          };

          mkNixosSystem = name: config: inputs.nixpkgs.lib.nixosSystem {
            specialArgs = {
              inherit inputs;
              inherit (inputs.self) outputs;
            };
            modules = [
              inputs.home-manager.nixosModules.home-manager
              ./hosts/${config.hostname}
              (import ./modules/nixos)
              {
                programs.slack.enable = config.programs.slack;
                programs.discord.enable = config.programs.discord;
                home-manager = {
                  useUserPackages = false; # TODO on reinstall
                  extraSpecialArgs = {
                    inherit inputs;
                    inherit (inputs.self) outputs;
                    inherit (inputs.self.nixosConfigurations.${name}.config.networking) hostName;
                  };
                  backupFileExtension = ".hm-backup";
                  users.kristian = { ... }: {
                    nixpkgs.config.allowUnfree = true;
                    imports = [
                      ./home/${config.hostname}
                    ];
                  };
                };
              }
            ] ++ (config.extraModules or []);
          };
        in
        {
          lib = inputs.nixpkgs.lib // inputs.home-manager.lib;

          overlays = {
            default = import ./overlay { inherit inputs; inherit (inputs.self) outputs; };
          };

          templates = import ./templates;

          nixosConfigurations = inputs.nixpkgs.lib.mapAttrs mkNixosSystem hostConfigs;
        };
    };
}
