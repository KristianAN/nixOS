{
  description = "Kristian start with NixOS flakes";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    # flake-utils is a tool that helps you work with flakes
    flake-utils.url = "github:numtide/flake-utils";

    # Home Manager is a tool that helps you manage your dotfiles
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    copilot-el = {
      url = "github:zerolfx/copilot.el";
      flake = false;
    };

    scala-ts-mode = {
      url = "github:KaranAhlawat/scala-ts-mode";
      flake = false;
    };

    indent-bars = {
      url = "github:jdtsmith/indent-bars";
      flake = false;
    };

    dape = {
      url = "github:svaante/dape";
      flake = false;
    };

    scala-cli-repl = {
      url = "github:ag91/scala-cli-repl";
      flake = false;
    };

    myNeovimFlake = {
      url = "github:KristianAN/neovim-flake";
      flake = true;
    };

  };

  outputs =
    { nixpkgs
    , flake-utils
    , myNeovimFlake
    , home-manager
    , ...
    } @ inputs:
    let
      forAllSystems = nixpkgs.lib.genAttrs systems;
      systems = [
        "x86_64-linux"
      ];
    in
    rec {
      overlays = {
        default = import ./overlay { inherit inputs; };
      };

      # NixOS modules

      nixosModules = import ./modules/nixos;
      homeManagerModules = import ./modules/home-manager;


      # Overlays
      legacyPackages = forAllSystems (system:
        import inputs.nixpkgs {
          inherit system;
          overlays = builtins.attrValues overlays;
          config.allowUnfree = true;
        });

      # NixOS configuration, callable for each system
      nixosConfigurations = {

        # Sky
        sky = nixpkgs.lib.nixosSystem {

          specialArgs = { inherit inputs; };
          modules = [
            ./hosts/sky
            nixosModules
            {
              programs.slack.enable = false;
              programs.citrix.enable = false;
              programs.discord.enable = false;
              programs.intellij.enable = false;
            }
          ];
        };

        # Rubble
        rubble = nixpkgs.lib.nixosSystem {

          specialArgs = { inherit inputs; };
          modules = [
            ./hosts/rubble
            nixosModules
            {
              programs.slack.enable = true;
              programs.citrix.enable = false;
              programs.discord.enable = true;
              programs.intellij.enable = true;
            }
          ];
        };

        # Chase 
        chase = nixpkgs.lib.nixosSystem {
          specialArgs = { inherit inputs; };
          modules = [
            ./hosts/chase
            nixosModules
            {
              programs.slack.enable = true;
              programs.citrix.enable = true;
              programs.discord.enable = true;
              programs.intellij.enable = true;
            }
          ];
        };
      };

      # Home Manager configuration, callable for each system
      homeConfigurations = {
        # Sky
        "kristian@sky" = home-manager.lib.homeManagerConfiguration {
          pkgs = legacyPackages.x86_64-linux;
          extraSpecialArgs = { inherit inputs; };
          modules = [
            ./home/sky
            homeManagerModules
          ];
        };


        # Rubble
        "kristian@rubble" = home-manager.lib.homeManagerConfiguration {
          pkgs = legacyPackages.x86_64-linux;
          extraSpecialArgs = { inherit inputs; };
          modules = [
            ./home/rubble
            homeManagerModules
          ];
        };

        "kristian@chase" = home-manager.lib.homeManagerConfiguration {
          pkgs = legacyPackages.x86_64-linux;
          extraSpecialArgs = { inherit inputs; };
          modules = [
            ./home/chase
            homeManagerModules
          ];
        };
      };
    };
}
