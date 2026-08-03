{ config, pkgs, ... }:
{
  services.pcscd.enable = false;

  security.pam = {
    # Verify on hardware after changing: pam_u2f is "sufficient", so a
    # missing or unenrolled key falls back to the normal password prompt.
    services = {
      lightdm.u2f.enable = true;
      login.u2f.enable = true;
      swaylock.u2f.enable = true;
    };
    u2f.enable = true;
  };

}
