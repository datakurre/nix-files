{ config, pkgs, ... }:
{
  environment.systemPackages = [ pkgs.openconnect ];
  home-manager.users.${config.user.name} = {
    services = {
      blueman-applet.enable = true;
      network-manager-applet.enable = true;
    };
  };
  networking = {
    firewall.enable = true;
    networkmanager = {
      dns = "systemd-resolved";
      enable = true;
    };
  };
  services = {
    resolved.enable = true;
    tailscale.enable = true;
  };
}
