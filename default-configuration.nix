{ config, pkgs, ... }:
{
  # Policy shared by all machines. Hardware-specific settings belong to
  # machines/<host>/manual.nix.
  boot.loader = {
    efi.canTouchEfiVariables = true;
    systemd-boot.enable = true;
  };
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
  };
  services.fwupd.enable = true;

  imports = [
    ./modules/nixos/env-base.nix
    ./modules/nixos/env-fonts.nix
    ./modules/nixos/env-locale.nix
    ./modules/nixos/env-network.nix
    ./modules/nixos/env-nix.nix
    ./modules/nixos/env-user.nix
    ./modules/nixos/hw-yubikey.nix
    ./modules/nixos/programs-obs.nix
    ./modules/nixos/programs-ssh.nix
    ./modules/nixos/programs-vim.nix
    ./modules/nixos/services-fineid.nix
    ./modules/nixos/services-virtualization.nix
    ./modules/nixos/services-river.nix
  ];
  home-manager.users.${config.user.name} = {
    imports = (import ./modules/home/default.nix) ++ [ ./modules/home/services-river.nix ];
  };
}
