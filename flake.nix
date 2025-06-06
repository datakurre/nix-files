{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
    home-manager.url = "github:rycee/home-manager/release-24.11";
  };
  outputs =
    {
      self,
      nixpkgs,
      nixpkgs-unstable,
      flake-utils,
      home-manager,
      ...
    }@inputs:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        unstable = import nixpkgs-unstable {
          overlays = [ (self: super: { }) ];
          inherit system;
        };
        pkgs = import nixpkgs {
          overlays = [ (self: super: { }) ];
          inherit system;
        };
      in
      {
        overlays.default = self: super: { };
        packages.discord = pkgs.discord;
        packages.homeConfigurations."atsoukka" = home-manager.lib.homeManagerConfiguration {
          modules = [
            {
              home.username = "atsoukka";
              home.homeDirectory = "/home/atsoukka";
              home.stateVersion = "24.11";
            }
            ./home-atsoukka.nix
          ];
          inherit pkgs;
        };
        formatter = pkgs.nixfmt-rfc-style;
      }
    )
    // {
      nixosConfigurations.albemuth = nixpkgs.lib.nixosSystem {
        modules = [
          (
            { ... }:
            {
              nixpkgs.overlays = [ self.overlays.x86_64-linux.default ];
              user.name = "datakurre";
              user.description = "Asko Soukka";
              user.home = "/home/datakurre";
            }
          )
          home-manager.nixosModules.home-manager
          ./machines/albemuth-x1g11
          ./default-configuration.nix
        ];
        system = "x86_64-linux";
      };
      nixosConfigurations.makondo = nixpkgs.lib.nixosSystem {
        modules = [
          (
            { ... }:
            {
              nixpkgs.overlays = [ self.overlays.x86_64-linux.default ];
              user.name = "atsoukka";
              user.description = "Asko Soukka";
              user.home = "/home/atsoukka";
            }
          )
          home-manager.nixosModules.home-manager
          ./machines/makondo-p7670
          ./default-configuration.nix
        ];
        system = "x86_64-linux";
      };
    };
}
