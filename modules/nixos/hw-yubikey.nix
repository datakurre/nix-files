{ config, pkgs, ... }:
{
  security.pam = {
    # Verify on hardware after changing: pam_u2f is "sufficient", so a
    # missing or unenrolled key falls back to the normal password prompt.
    services = {
      lightdm.u2fAuth = true;
      login.u2fAuth = true;
    };
    u2f.enable = true;
  };
  services.pcscd.enable = false;
}
