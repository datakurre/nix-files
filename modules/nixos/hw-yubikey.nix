{ config, pkgs, ... }:
{
  # scdaemon runs with disable-ccid (see modules/home/programs-ssh.nix), so the card is
  # reached through PC/SC rather than GnuPG's internal CCID driver. Exactly one driver
  # owns the reader, which is what settles the "card busy / not found" fights -- turning
  # pcscd off instead just leaves scdaemon talking to a socket nobody is listening on
  # ("Service is not running"). OpenSC, the Firefox FINEID security device in
  # modules/home/services-fineid.nix, is a PC/SC client too and needs this daemon.
  services.pcscd.enable = true;

  security.pam = {
    # Verify on hardware after changing: pam_u2f is "sufficient", so a
    # missing or unenrolled key falls back to the normal password prompt.
    services = {
      greetd.u2f.enable = true;
      login.u2f.enable = true;
      swaylock.u2f.enable = true;
    };
    u2f.enable = true;
  };

}
