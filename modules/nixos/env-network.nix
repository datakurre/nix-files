{ config, pkgs, ... }:
{
  environment.systemPackages = [ pkgs.openconnect ];
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
