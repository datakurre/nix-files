{
  config,
  pkgs,
  lib,
  ...
}:
{
  home-manager.users.${config.user.name} = {
    home.packages = [ pkgs.discord ];
  };
}
