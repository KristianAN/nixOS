{
  description = "Kristian start with NixOS flakes";
  inputs = {

    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    disko = {
      url = "github:nix-community/disko/latest";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # flake-utils is a tool that helps you work with flakes
    flake-utils.url = "github:numtide/flake-utils";

    # Home Manager is a tool that helps you manage your dotfiles
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    impermanence.url = "github:nix-community/impermanence";
    myNeovimFlake = {
      # Remove this please, puke...
      url = "github:KristianAN/neovim-flake";
      flake = true;
    };
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    zen-browser = {
      url = "github:youwen5/zen-browser-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    indent-bars = {
      url = "github:jdtsmith/indent-bars";
      flake = false;
    };

  };

  outputs =
    { self
    , nixpkgs
    , home-manager
    , ...
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
      homeManagerModules = import ./modules/home-manager; # You can remove this...
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
                useGlobalPkgs = true;
                useUserPackages = true;
                extraSpecialArgs = { inherit inputs outputs; inherit (inputs.self.nixosConfigurations.sky.config.networking) hostName; };
                backupFileExtension = ".hm-backup";
                users.kristian = { ... }: {
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
                useGlobalPkgs = true;
                useUserPackages = true;
                extraSpecialArgs = { inherit inputs outputs; inherit (inputs.self.nixosConfigurations.rubble.config.networking) hostName; };
                backupFileExtension = ".hm-backup";
                users.kristian = { ... }: {
                  imports = [
                    ./home/rubble
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
                useGlobalPkgs = true;
                useUserPackages = true;
                extraSpecialArgs = { inherit inputs outputs; inherit (inputs.self.nixosConfigurations.chase.config.networking) hostName; };
                backupFileExtension = ".hm-backup";
                users.kristian = { ... }: {
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
