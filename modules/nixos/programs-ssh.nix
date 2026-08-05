{ config, pkgs, ... }:
{
  # The GnuPG agent (with SSH support) is managed per-user by Home Manager
  # (modules/home/programs-ssh.nix) so it works identically on non-NixOS hosts.
  programs.ssh.startAgent = false;
  services.openssh = {
    settings = {
      X11Forwarding = true;
      PermitRootLogin = "no";
      PasswordAuthentication = false;
      LoginGraceTime = 60;
      HostKeyAlgorithms = "+ssh-rsa,+ssh-ed25519";
      PubkeyAcceptedAlgorithms = "+ssh-rsa,+ssh-ed25519";
    };
  };
}
