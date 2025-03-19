{ config, pkgs, ... }:
{
  home-manager.users.${config.user.name} = {
    programs.obs-studio.enable = true;
  };
  networking.firewall.allowedTCPPorts = [
    4444 # obs-websocket
  ];
}
