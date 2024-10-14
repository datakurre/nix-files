{
  config,
  pkgs,
  lib,
  ...
}:
{
  home-manager.users.${config.user.name} = {
    programs.vscode = {
      enable = true;
      package = pkgs.vscodium;
    };
  };
  networking.firewall.allowedTCPPortRanges = [
    {
      from = 5990; # vscode
      to = 5999; # vscode
    }
  ];
}
