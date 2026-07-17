{ config, pkgs, ... }:
{
  options = {
    user.name = pkgs.lib.mkOption { type = pkgs.lib.types.str; };
    user.description = pkgs.lib.mkOption { type = pkgs.lib.types.str; };
    user.home = pkgs.lib.mkOption { type = pkgs.lib.types.str; };
  };
  config = {
    users.users.${config.user.name} = {
      description = config.user.description;
      home = config.user.home;
      isNormalUser = true;
      shell = pkgs.nushell;
      extraGroups = [
        "audio"
        "dialout"
        "input"
        "systemd-journal"
        "video"
        "wheel"
      ];
      uid = 1000;
    };
  };
}
