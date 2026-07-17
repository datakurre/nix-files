{ config, pkgs, ... }:
{
  networking.firewall.allowedTCPPorts = [
    4444
  ];
}
