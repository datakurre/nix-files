{ config, pkgs, ... }:
{
  security = {
    polkit.enable = true;
    sudo = {
      enable = true;
      wheelNeedsPassword = true;
    };
  };
  home-manager = {
    backupFileExtension = ".bak";
    useGlobalPkgs = true;
    users.${config.user.name} = {
      programs.home-manager.enable = true;
      home.stateVersion = config.system.stateVersion;
    };
  };
  nixpkgs.config.allowUnfree = true;
}
