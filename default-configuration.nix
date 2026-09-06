{ config, pkgs, ... }:
{
  # Policy shared by all machines. Hardware-specific settings belong to
  # machines/<host>/manual.nix.
  boot.loader = {
    efi.canTouchEfiVariables = true;
    systemd-boot.enable = true;
    # Without a limit, systemd-boot copies every generation's kernel and initrd to
    # the ESP and never removes any, so /boot fills up and a rebuild dies with
    # "No space left on device" while installing the bootloader -- long before the
    # Nix store itself is under any pressure.
    #
    # Sized against makondo's 236M ESP (/dev/nvme0n1p2), which is the smaller of
    # the two: bzImage is 12.2 MiB and the zstd initrd 41.0 MiB, so a generation
    # with its own kernel costs ~53 MiB. The builder writes the new generation
    # before pruning old ones, so the peak is (limit + 1) generations: at 3 that is
    # ~213 MiB, just inside 236 MiB. Do not raise this without re-checking that
    # arithmetic -- 4 would not fit. In practice most generations share a kernel
    # and initrd (any config-only change reuses both), so the peak is rare.
    #
    # The real fix is a bigger ESP; 236M is below what NixOS wants and leaves no
    # room for a kernel bump. That means repartitioning around the LUKS container
    # on nvme0n1p1, which is a separate job.
    systemd-boot.configurationLimit = 3;
  };
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
  };
  services.fwupd.enable = true;
  programs.chromium.enable = true;
  environment.systemPackages = [
    pkgs.chromium
    pkgs.jfrog-cli
    pkgs.zip
    pkgs.unzip
  ];

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
    ./modules/nixos/services-minecraft-bedrock.nix
  ];
  home-manager.users.${config.user.name} = {
    imports = (import ./modules/home/default.nix) ++ [ ./modules/home/services-river.nix ];
  };
}
