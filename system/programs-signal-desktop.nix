{ config, pkgs, ... }:
{
  home-manager.users.${config.user.name} = {
    home.packages = [ pkgs.signal-desktop ];
  };
}
