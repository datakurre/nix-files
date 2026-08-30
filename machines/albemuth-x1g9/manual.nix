{ config, pkgs, ... }:
{
  boot.initrd = {
    availableKernelModules = [ "kvm" ];
    luks.devices."luks-05e3771a-f128-4db5-b054-8804bd6b7c14".device =
      "/dev/disk/by-uuid/05e3771a-f128-4db5-b054-8804bd6b7c14";
  };
  hardware.ipu6 = {
    enable = true;
    platform = "ipu6ep";
  };
  environment.systemPackages = [
    pkgs.acpi
    # Calibre
    (pkgs.calibre.override { unrarSupport = true; })
  ];
  networking.hostName = "albemuth";
  services = {
    thinkfan.enable = true;
  };
  system.stateVersion = "24.05";

  # Waydroid
  # virtualisation.waydroid.enable = true;
  # virtualisation.waydroid.package = pkgs.waydroid-nftables;

  # Enable the xpadneo driver for Xbox One/Series controllers
  hardware.xpadneo.enable = true;

  # Calibre
  services.udisks2.enable = true;

  # Roblox
  services.flatpak.enable = true;

  # Minecraft Bedrock dedicated server
  services.minecraft-bedrock.servers."crazy-land" = {
    enable = false;
    openFirewall = true;
    serverProperties = {
      server-name = "Crazy Land";
      level-name = "crazy_land";
      online-mode = false;
      enable-lan-visibility = true;
      gamemode = "survival";
      difficulty = "easy";
      force-gamemode = true;
    };
  };

  home-manager.users.${config.user.name}.services.kanshi = {
    enable = true;
    settings = [
      {
        profile.name = "internal";
        profile.outputs = [
          {
            criteria = "eDP-1";
            scale = 2.0;
          }
        ];
      }
    ];
  };

}
