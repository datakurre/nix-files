{ config, pkgs, ... }:
{
  home-manager.users.${config.user.name} = {
    home = {
      packages = [ pkgs.pam_u2f ];
    };
  };
  security.pam = {
    services.${config.user.name}.u2fAuth = true;
    u2f.enable = true;
  };
  services.pcscd.enable = false;
}
