{
  config,
  ...
}:
{
  home-manager.users.${config.home.username} = {
    imports = [ ../home-manager/programs-vscode.nix ];
  };
  networking.firewall.allowedTCPPortRanges = [
    {
      from = 5990; # vscode
      to = 5999; # vscode
    }
  ];
}
