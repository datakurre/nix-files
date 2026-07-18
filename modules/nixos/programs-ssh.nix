{ config, pkgs, ... }:
{
  # The GnuPG agent (with SSH support) is managed per-user by Home Manager
  # (modules/home/programs-ssh.nix) so it works identically on non-NixOS hosts.
  programs.ssh.startAgent = false;
  services.openssh = {
    allowSFTP = true;
    extraConfig = ''
      HostkeyAlgorithms +ssh-rsa +ed25519
      PubkeyAcceptedAlgorithms +ssh-rsa +ed25519
    '';
    settings = {
      X11Forwarding = true;
      PermitRootLogin = false;
      PasswordAuthentication = false;
      LoginGraceTime = 0;
    };
  };
}
