{ config, pkgs, ... }:
{
  boot.initrd = {
    availableKernelModules = [ "kvm" ];
    luks.devices."luks-05e3771a-f128-4db5-b054-8804bd6b7c14".device =
      "/dev/disk/by-uuid/05e3771a-f128-4db5-b054-8804bd6b7c14";
  };
  boot.kernelPackages =
    (pkgs.linuxPackagesFor (
      pkgs.linux_6_12.override {
        argsOverride = rec {
          src = pkgs.fetchurl {
            url = "mirror://kernel/linux/kernel/v6.x/linux-${version}.tar.xz";
            sha256 = "sha256-AZOx2G3TcuyJG655n22iDe7xb8GZ8wCApOqd6M7wxhk=";
          };
          version = "6.12.1";
          modDirVersion = "6.12.1";
        };
      }
    )).extend
      (
        _: _: {
          ipu6-drivers = config.boot.kernelPackages.callPackage ./ipudrivers.nix { };
        }
      );
  hardware.ipu6.enable = true;
  hardware.ipu6.platform = "ipu6ep";
  boot.loader = {
    efi.canTouchEfiVariables = true;
    systemd-boot.enable = true;
  };
  environment.systemPackages = [ pkgs.acpi ];
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
  };
  home-manager.users.${config.user.name} = {
    services.stalonetray.config.icon_size = 48;
    xresources.properties."Xcursor.size" = "64";
  };
  hardware.keyboard.qmk.enable = true;
  networking.hostName = "albemuth";
  services = {
    fwupd.enable = true;
    thinkfan.enable = true;
    xserver.displayManager.xserverArgs = [ "-dpi 196" ];
  };
  system.stateVersion = "24.05";
}
