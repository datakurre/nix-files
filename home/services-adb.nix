{ config, pkgs, ... }:
{
  users.users.${config.user.name} = {
    extraGroups = [ "adbusers" ];
  };
  programs.adb.enable = true;
  services.udev.packages = [ pkgs.android-udev-rules ];
}
