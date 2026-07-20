{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/master";
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
    home-manager.url = "github:nix-community/home-manager/release-26.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
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

      swaylockXjackOverlay = final: prev: {
        swaylock = prev.swaylock.overrideAttrs (old: {
          patches = (old.patches or [ ]) ++ [
            ./pkgs/swaylock-xjack/patches/0001-add-effect-api.patch
            ./pkgs/swaylock-xjack/patches/0002-add-xjack-effect.patch
            ./pkgs/swaylock-xjack/patches/0003-add-xjack-cli-options.patch
            ./pkgs/swaylock-xjack/patches/0004-incremental-render.patch
            ./pkgs/swaylock-xjack/patches/0005-custom-source-text.patch
          ];
        });
      };

      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          unstableOverlay
          swaylockXjackOverlay
        ];
      };

      mkNixos =
        host:
        {
          name,
          description,
          home,
        }:
        nixpkgs.lib.nixosSystem {
          inherit system;
          modules = [
            (
              { ... }:
              {
                nixpkgs.overlays = [
                  self.overlays.default
                  swaylockXjackOverlay
                ];
                user.name = name;
                user.description = description;
                user.home = home;
              }
            )
            home-manager.nixosModules.home-manager
            host
            ./default-configuration.nix
          ];
        };
    in
    {
      overlays.default = unstableOverlay;

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
          overlays = [
            unstableOverlay
            swaylockXjackOverlay
          ];
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

      nixosConfigurations.albemuth = mkNixos ./machines/albemuth-x1g9 {
        name = "datakurre";
        description = "Asko Soukka";
        home = "/home/datakurre";
      };

      nixosConfigurations.makondo = mkNixos ./machines/makondo-p7670 {
        name = "atsoukka";
        description = "Asko Soukka";
        home = "/home/atsoukka";
      };
    };
}
