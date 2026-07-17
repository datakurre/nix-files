{ config, pkgs, ... }:
{
  security.pam = {
    services.${config.user.name}.u2fAuth = true;
    u2f.enable = true;
  };
  services.pcscd.enable = false;
}
