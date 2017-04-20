{ config, lib, pkgs, ... }:

{
  imports =
    [ <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ];

  # Minimal list of modules to use the efi system partition and the YubiKey
  boot.initrd.availableKernelModules = [
     "nls_cp437"
     "nls_iso8859-1"
     "usbhid"
     "vfat"
  ] ++ [
     "ahci"
     "ehci_pci"
     "rtsx_pci_sdmmc"
     "sd_mod"
  ];
  boot.blacklistedKernelModules = [ ];
  boot.extraModulePackages = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.kernelParams = [ "usbcore.autosuspend=-1" ];

  # Crypto setup, set modules accordingly
  boot.initrd.luks.cryptoModules = [ "aes" "xts" "sha512" ];

  # Enable support for the YubiKey PBA
  boot.initrd.luks.yubikeySupport = true;

  # Configuration to use your Luks device
  boot.initrd.luks.devices = [ {
    name = "luksroot";
    device = "/dev/sda2";
    preLVM = true;
    yubikey = {
      storage = {
        device = "/dev/sda1";
      };
    };
  } ];

  # File systems
  swapDevices = [ { device = "/dev/partitions/swap"; } ];

  fileSystems."/" = {
    label = "root";
    device = "/dev/partitions/fsroot";
    fsType = "btrfs";
    options = [ "subvol=root" ];
  };

  fileSystems."/home" = {
    label = "home";
    device = "/dev/partitions/fsroot";
    fsType = "btrfs";
    options = [ "subvol=home" ];
  };

  nix.maxJobs = 8;
}
