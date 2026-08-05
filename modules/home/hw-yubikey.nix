{ pkgs, ... }:
{
  # Note: pam_u2f has no effect without system PAM config (which is handled on NixOS).
  home.packages = [ pkgs.pam_u2f ];
}
