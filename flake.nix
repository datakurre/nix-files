{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/master";
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
    home-manager.url = "github:nix-community/home-manager/release-26.05";
  };
  outputs =
    {
      self,
      nixpkgs,
      nixpkgs-unstable,
      home-manager,
      ...
    }@inputs:
    let
      system = "x86_64-linux";

      unstableOverlay = final: prev: {
        unstable = import nixpkgs-unstable {
          system = prev.stdenv.hostPlatform.system;
          config = prev.config;
        };
      };

      pkgs = import nixpkgs {
        inherit system;
        config = {
          allowUnfreePredicate = pkg: builtins.elem (nixpkgs.lib.getName pkg) [ "discord" ];
        };
        overlays = [ unstableOverlay ];
      };
    in
    {
      overlays.default = unstableOverlay;

      packages.${system}.discord = pkgs.unstable.discord;

      formatter.${system} = pkgs.nixfmt;

      homeConfigurations."atsoukka" = home-manager.lib.homeManagerConfiguration {
        pkgs = import nixpkgs {
          inherit system;
          config = {
            allowUnfreePredicate =
              pkg:
              builtins.elem (nixpkgs.lib.getName pkg) [
                "corefonts"
                "vagrant"
              ];
          };
          overlays = [ unstableOverlay ];
        };
        modules = [
          {
            home.username = "atsoukka";
            home.homeDirectory = "/home/atsoukka";
            home.stateVersion = "24.11";
          }
          ./home-configuration.nix
        ];
      };

      nixosConfigurations.albemuth = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          (
            { ... }:
            {
              nixpkgs.overlays = [ self.overlays.default ];
              user.name = "datakurre";
              user.description = "Asko Soukka";
              user.home = "/home/datakurre";
            }
          )
          home-manager.nixosModules.home-manager
          ./machines/albemuth-x1g9
          ./default-configuration.nix
        ];
      };

      nixosConfigurations.makondo = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          (
            { ... }:
            {
              nixpkgs.overlays = [ self.overlays.default ];
              user.name = "atsoukka";
              user.description = "Asko Soukka";
              user.home = "/home/atsoukka";
            }
          )
          home-manager.nixosModules.home-manager
          ./machines/makondo-p7670
          ./default-configuration.nix
        ];
      };
    };
}
