{
  config,
  pkgs,
  lib,
  ...
}:
{
  options = {
    user.name = lib.mkOption { type = lib.types.str; };
    user.description = lib.mkOption { type = lib.types.str; };
    user.home = lib.mkOption { type = lib.types.str; };
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
