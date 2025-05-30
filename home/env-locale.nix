{ config, pkgs, ... }:
{
  console = {
    font = "Lat2-Terminus16";
    keyMap = "fi";
  };
  home-manager.users.${config.user.name} = {
    home.file = {
      ".docutils.conf".source = ./env-locale-docutils.conf;
      ".editorconfig".source = ./env-locale-editorconfig.conf;
    };
  };
  i18n = {
    defaultLocale = "fi_FI.UTF-8";
    extraLocaleSettings = {
      LC_ADDRESS = "fi_FI.UTF-8";
      LC_IDENTIFICATION = "fi_FI.UTF-8";
      LC_MEASUREMENT = "fi_FI.UTF-8";
      LC_MONETARY = "fi_FI.UTF-8";
      LC_NAME = "fi_FI.UTF-8";
      LC_NUMERIC = "fi_FI.UTF-8";
      LC_PAPER = "fi_FI.UTF-8";
      LC_TELEPHONE = "fi_FI.UTF-8";
      LC_TIME = "fi_FI.UTF-8";
    };
  };
  services.xserver.xkb.layout = "fi";
  time.timeZone = "Europe/Helsinki";
}
