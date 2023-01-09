{
  description = "Home-manager configuration";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.11";
    unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    # for pkgs
    nixgl.url = "github:guibou/nixGL";
    nixgl.inputs.nixpkgs.follows = "nixpkgs";
    nixgl.inputs.flake-utils.follows = "flake-utils";
    rcc.url = "github:robocorp/rcc/c687438a726bd417437f4fbffae9db61f0010a87";  # v11.36.5
    rcc.flake = false;
    zbctl.url = "github:camunda/zeebe/clients/go/v8.1.5";
    zbctl.flake = false;
  };

  outputs = { self, flake-utils, nixpkgs, unstable, home-manager, nixgl, rcc, zbctl }:
    let
      localOverlay = system: prev: final: {
        nixGL = nixgl.packages.${system}.default;
        inherit (import unstable {
          inherit system;
          config = {
            allowUnfreePredicate = pkg:
            builtins.elem ((import nixpkgs { inherit system; }).lib.getName pkg) [
              "teams"
            ];
          };
        })
        bitwarden-cli
        teams
        vscode
        ;
        inherit(self.packages.${system})
        camunda-modeler
        mockoon
        zbctl
        rcc
        rccFHSUserEnv
        ;
      };

      pkgsForSystem = system: import nixpkgs {
        overlays = [
          (localOverlay system)
        ];
        inherit system;
      };

      mkHomeConfiguration = args: home-manager.lib.homeManagerConfiguration (rec {
        pkgs = pkgsForSystem args.system or "x86_64-linux";
        modules = [ ./home.nix { home = { inherit (args) username homeDirectory; }; } ];
        extraSpecialArgs = {
          localOverlay = localOverlay (args.system or "x86_64-linux");
        } // args.extraSpecialArgs;
      });

    in flake-utils.lib.eachSystem [ "x86_64-linux" ] (system: rec {
      legacyPackages = pkgsForSystem system;
  }) // {
    # non-system suffixed items should go here
    overlay = localOverlay;
    nixosModules.home = import ./home.nix; # attr set or list

    homeConfigurations.atsoukka = mkHomeConfiguration rec {
      username = "atsoukka";
      homeDirectory = "/home/atsoukka";
      extraSpecialArgs = {
        inherit username homeDirectory;
        withGUI = true;
        isDesktop = true;
        networkInterface = "em1";
      };
    };

    inherit home-manager;
  } // flake-utils.lib.eachDefaultSystem (system: let pkgs = nixpkgs.legacyPackages.${system}; in {
    packages.camunda-modeler = pkgs.callPackage ./pkgs/camunda-modeler {};
    packages.mockoon = pkgs.callPackage ./pkgs/mockoon {};
    packages.zbctl = pkgs.callPackage ./pkgs/zbctl { src = zbctl; version = "v8.1.5"; };
    packages.rcc = pkgs.callPackage ./pkgs/rcc/rcc.nix { src = rcc; version = "v11.36.5"; };
    packages.rccFHSUserEnv = pkgs.callPackage ./pkgs/rcc { src = rcc; version = "v11.36.5"; };
  });
}
