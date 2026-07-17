{ config, pkgs, ... }:
{
  programs = {
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };
    ssh.startAgent = false;
  };
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
