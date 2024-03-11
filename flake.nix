{
  description = "Kristian start with NixOS flakes";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    # flake-utils is a tool that helps you work with flakes
    flake-utils.url = "github:numtide/flake-utils";

    # Home Manager is a tool that helps you manage your dotfiles
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    # myNeovimFlake = {
    #  url = "github:KristianAN/neovim-flake";
    #  flake = true;
    #};
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , myNeovimFlake
    , home-manager
    , ...
    } @ inputs:
    let
      inherit (self) outputs;
      lib = nixpkgs.lib // home-manager.lib;
      systems = [ "x86_64-linux" "aarch64-linux" ];
      forEachSystem = f: lib.genAttrs systems (system: f pkgsFor.${system});
      pkgsFor = lib.genAttrs systems (system:
        import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        });
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
      devShells = forEachSystem (pkgs:
        import ./shell.nix {
          inherit pkgs;
          buildInputs = with pkgs; [
          ];
        });

      nixosConfigurations = {
        sky = lib.nixosSystem {
          specialArgs = { inherit inputs outputs; };
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
        rubble = lib.nixosSystem {
          specialArgs = { inherit inputs outputs; };
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
        chase = lib.nixosSystem {
          specialArgs = { inherit inputs outputs; };
          modules = [
            ./hosts/chase
            nixosModules
            {
              programs.slack.enable = true;
              programs.citrix.enable = false;
              programs.discord.enable = true;
              programs.intellij.enable = true;
            }
          ];
        };
      };
      homeConfigurations = {
        "kristian@sky" = lib.homeManagerConfiguration {
          pkgs = pkgsFor.x86_64-linux;
          extraSpecialArgs = {
            inherit inputs outputs;
          };
          modules = [
            ./home/sky
            homeManagerModules
          ];
        };
        "kristian@rubble" = lib.homeManagerConfiguration {
          pkgs = pkgsFor.x86_64-linux;
          extraSpecialArgs = {
            inherit inputs outputs;
          };
          modules = [
            ./home/rubble
            homeManagerModules
          ];
        };
        "kristian@chase" = lib.homeManagerConfiguration {
          pkgs = pkgsFor.x86_64-linux;
          extraSpecialArgs = {
            inherit inputs outputs;
          };
          modules = [
            ./home/chase
            homeManagerModules
          ];
        };
      };
    };
}
