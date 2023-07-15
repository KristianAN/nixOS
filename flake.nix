{
  description = "Kristian start with NixOS flakes";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    # flake-utils is a tool that helps you work with flakes
    flake-utils.url = "github:numtide/flake-utils";

    # Home Manager is a tool that helps you manage your dotfiles
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    home-manager,
    ...
  } @ inputs: let
    forAllSystems = nixpkgs.lib.genAttrs systems;
    systems = [
      "x86_64-linux"
      "i86_64-darwin"
    ];
  in rec {
    overlays = {
      default = import ./overlay {inherit inputs;};
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
      # Desktop, named janMagnus after the legend
      janMagnus = nixpkgs.lib.nixosSystem {
        specialArgs = {inherit inputs;};
        modules = [
          ./hosts/janMagnus
          nixosModules
          {
            programs.slack.enable = true;
            programs.teams.enable = true;
            programs.citrix.enable = true;
            programs.discord.enable = true;
            programs.intellij.enable = true;
          }
        ];
      };

      # Laptop -> TODO: Add modules for laptop
    };

    # Home Manager configuration, callable for each system
    homeConfigurations = {
      "kristian@desktop" = home-manager.lib.homeManagerConfiguration {
        pkgs = legacyPackages.x86_64-linux;
        extraSpecialArgs = {inherit inputs;};
        modules = [
          ./home/kristian
          homeManagerModules
        ];
      };

      # Laptop -> TODO: Add modules for laptop
    };
  };
}
