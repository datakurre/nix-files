{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/master";
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
    home-manager.url = "github:nix-community/home-manager/release-26.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nix-vscode-extensions.url = "github:nix-community/nix-vscode-extensions";
    agent-sandbox.url = "github:datakurre/agent-sandbox";
    bpmn-to-image.url = "github:datakurre/bpmn-to-image";
    bpmn-to-image.inputs.nixpkgs.follows = "nixpkgs";
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
            ./pkgs/swaylock-xjack/patches/0006-weighted-word-slots.patch
            ./pkgs/swaylock-xjack/patches/0007-restart-on-scroll.patch
            ./pkgs/swaylock-xjack/patches/0008-performance-fixes.patch
          ];
        });
      };

      mkAgentSandboxOverlay = selinux: final: prev: {
        agent-sandbox = prev.symlinkJoin {
          name = "agent-sandbox";
          paths = [ inputs.agent-sandbox.packages.${prev.stdenv.hostPlatform.system}.default ];
          nativeBuildInputs = [ prev.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/agent-sandbox --add-flags "--nix --git --ssh --gpg --devenv --ports --mounts --proxy --workspace${
              if selinux then " --selinux" else ""
            }"
          '';
        };
      };

      agentSandboxOverlay = mkAgentSandboxOverlay false;
      agentSandboxSelinuxOverlay = mkAgentSandboxOverlay true;

      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          inputs.nix-vscode-extensions.overlays.default
          inputs.bpmn-to-image.overlays.default
          unstableOverlay
          agentSandboxOverlay
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
                  inputs.nix-vscode-extensions.overlays.default
                  inputs.bpmn-to-image.overlays.default
                  self.overlays.default
                  swaylockXjackOverlay
                  agentSandboxOverlay
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
      overlays.swaylock-xjack = swaylockXjackOverlay;
      overlays.agent-sandbox = agentSandboxOverlay;
      overlays.bpmn-to-image = inputs.bpmn-to-image.overlays.default;

      formatter.${system} = pkgs.writeShellApplication {
        name = "formatter";
        runtimeInputs = [
          pkgs.treefmt
          pkgs.nixfmt
        ];
        text = ''
          treefmt "$@"
        '';
      };

      homeConfigurations."atsoukka" = home-manager.lib.homeManagerConfiguration {
        pkgs = import nixpkgs {
          inherit system;
          config = {
            allowUnfreePredicate =
              pkg:
              builtins.elem (nixpkgs.lib.getName pkg) [
                "corefonts"
                "discord"
                "vagrant"
              ];
          };
          overlays = [
            inputs.nix-vscode-extensions.overlays.default
            inputs.bpmn-to-image.overlays.default
            unstableOverlay
            swaylockXjackOverlay
            agentSandboxSelinuxOverlay
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
