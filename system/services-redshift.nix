{ config, pkgs, ... }:
{
  home-manager.users.${config.user.name} = {
    services.redshift = {
      enable = true;
      latitude = "25.4449";
      longitude = "62.1435";
      settings.redshift = {
        brightness-day = "1.0";
        brightness-night = "0.7";
        temp-day = 5500;
        temp-night = 3700;
      };
    };
  };
}
