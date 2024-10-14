{ config, pkgs, ... }:
{
  security.sudo = {
    enable = true;
    wheelNeedsPassword = true;
  };
  home-manager.users.${config.user.name} = {
    programs.home-manager.enable = true;
    home.stateVersion = config.system.stateVersion;
  };
}
