{
  description = "Kristian start with NixOS flakes";
  inputs = {

    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    # flake-utils is a tool that helps you work with flakes
    flake-utils.url = "github:numtide/flake-utils";

    # Home Manager is a tool that helps you manage your dotfiles
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    myNeovimFlake = {
      url = "github:KristianAN/neovim-flake";
      flake = true;
    };

    indent-bars = {
      url = "github:jdtsmith/indent-bars";
      flake = false;
    };

  };

  outputs =
    {
      self,
      nixpkgs,
      home-manager,
      ...
    }@inputs:
    let
      inherit (self) outputs;
      lib = nixpkgs.lib // home-manager.lib;
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
      forEachSystem = f: lib.genAttrs systems (system: f pkgsFor.${system});
      pkgsFor = lib.genAttrs systems (
        system:
        import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        }
      );
      nixosModules = import ./modules/nixos;
      homeManagerModules = import ./modules/home-manager;
    in
    {
      inherit lib;
      overlays = {
        default = import ./overlay { inherit inputs outputs; };
      };
      templates = import ./templates;
      packages = forEachSystem (pkgs: import ./pkgs { inherit pkgs; });
      devShells = forEachSystem (
        pkgs:
        import ./shell.nix {
          inherit pkgs;
          buildInputs = with pkgs; [ ];
        }
      );

      nixosConfigurations = {
        sky = lib.nixosSystem {
          specialArgs = {
            inherit inputs outputs;
          };
          modules = [
            inputs.home-manager.nixosModules.home-manager
            ./hosts/sky
            nixosModules
            {
              programs.slack.enable = true;
              programs.discord.enable = true;
              home-manager = {
                useUserPackages = false; # TODO on reinstall
                extraSpecialArgs = { inherit inputs outputs; };
                backupFileExtension = ".hm-backup";
                users.kristian = { ... }: {
                  nixpkgs.config.allowUnfree = true;
                  imports = [
                    ./home/sky
                  ];
                };
              };
            }
          ];
        };
        rubble = lib.nixosSystem {
          specialArgs = {
            inherit inputs outputs;
          };
          modules = [
            inputs.home-manager.nixosModules.home-manager
            ./hosts/rubble
            nixosModules
            {
              programs.slack.enable = true;
              programs.discord.enable = true;
              home-manager = {
                useUserPackages = false; # TODO on reinstall
                extraSpecialArgs = { inherit inputs outputs; };
                backupFileExtension = ".hm-backup";
                users.kristian = { ... }: {
                  nixpkgs.config.allowUnfree = true;
                  imports = [
                    ./home/rubble
                  ];
                };
              };
            }
          ];
        };
        everest = lib.nixosSystem {
          specialArgs = {
            inherit inputs outputs;
          };
          modules = [
            inputs.home-manager.nixosModules.home-manager
            ./hosts/everest
            nixosModules
            {
              programs.slack.enable = true;
              programs.discord.enable = true;
              home-manager = {
                useUserPackages = false; # TODO on reinstall
                extraSpecialArgs = { inherit inputs outputs; };
                backupFileExtension = ".hm-backup";
                users.kristian = { ... }: {
                  nixpkgs.config.allowUnfree = true;
                  imports = [
                    ./home/everest
                  ];
                };
              };
            }
          ];
        };
        chase = lib.nixosSystem {
          specialArgs = {
            inherit inputs outputs;
          };
          modules = [
            inputs.home-manager.nixosModules.home-manager
            ./hosts/chase
            nixosModules
            {
              programs.slack.enable = true;
              programs.discord.enable = true;
              home-manager = {
                useUserPackages = false; # TODO on reinstall
                extraSpecialArgs = { inherit inputs outputs; };
                backupFileExtension = ".hm-backup";
                users.kristian = { ... }: {
                  nixpkgs.config.allowUnfree = true;
                  imports = [
                    ./home/chase
                  ];
                };
              };
            }
          ];
        };
      };
    };
}
