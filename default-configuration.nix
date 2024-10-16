{ config, pkgs, ... }:
{
  imports = [
    ./modules/env-base.nix
    ./modules/env-fonts.nix
    ./modules/env-locale.nix
    ./modules/env-network.nix
    ./modules/env-nix.nix
    ./modules/env-user.nix
    ./modules/hw-marble-mouse.nix
    ./modules/hw-yubikey.nix
    ./modules/programs-discord.nix
    ./modules/programs-git.nix
    ./modules/programs-minecraft.nix
    ./modules/programs-obs.nix
    ./modules/programs-shell.nix
    ./modules/programs-ssh.nix
    ./modules/programs-vim.nix
    ./modules/programs-vscode.nix
    ./modules/services-adb.nix
    ./modules/services-battery-notifier.nix
    ./modules/services-cachix.nix
    ./modules/services-fineid.nix
    ./modules/services-redshift.nix
    ./modules/services-virtualization.nix
    ./modules/services-xmonad.nix
  ];
}
