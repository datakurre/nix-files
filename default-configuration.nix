{ config, pkgs, ... }:
{
  imports = [
    ./modules/nixos/env-base.nix
    ./modules/nixos/env-fonts.nix
    ./modules/nixos/env-locale.nix
    ./modules/nixos/env-network.nix
    ./modules/nixos/env-nix.nix
    ./modules/nixos/env-user.nix
    ./modules/nixos/hw-marble-mouse.nix
    ./modules/nixos/hw-yubikey.nix
    ./modules/nixos/programs-obs.nix
    ./modules/nixos/programs-ssh.nix
    ./modules/nixos/programs-vim.nix
    ./modules/nixos/services-fineid.nix
    ./modules/nixos/services-virtualization.nix
    ./modules/nixos/services-xmonad.nix
    # ./modules/nixos/services-adb.nix
    # ./modules/nixos/services-cachix.nix
  ];
  home-manager.users.${config.user.name} = {
    imports = import ./modules/home/default.nix;
  };
}
