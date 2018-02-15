{ config, lib, pkgs, ... }:

{
  imports =
    [ <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ];

  boot.initrd.availableKernelModules = [ "achi" "ehci_pci" "usb_storage" "xhci_hcd"
                                         # Minimal list of modules to use the efi system partition and the YubiKey
                                         "nls_cp437" "nls_iso8859-1" "usbhid" "vfat" ];
  boot.kernelModules = [ "kvm-intel" ];

  # Crypto setup, set modules accordingly
  boot.initrd.luks.cryptoModules = [ "aes" "xts" "sha512" ];

  # Enable support for the YubiKey PBA
  boot.initrd.luks.yubikeySupport = true;

  # Configuration to use your Luks device
  boot.initrd.luks.devices = [{
    name = "luksroot";
    device = "/dev/sda2";
    preLVM = true;
    yubikey = {
      storage = {
        device = "/dev/sda1";
      };
    };
  }];

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

  powerManagement.cpuFreqGovernor = "powersave";
}
