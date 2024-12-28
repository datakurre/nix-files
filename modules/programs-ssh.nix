{ config, pkgs, ... }:
{
  home-manager.users.${config.user.name} = {
    programs = {
      bash.bashrcExtra = ''
        export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
      '';
      nushell.envFile.text = ''
        ''$env.SSH_AUTH_SOCK = $"(gpgconf --list-dirs agent-ssh-socket)"
      '';
      ssh.enable = true;
    };
    home.packages = [ pkgs.gnupg ];
    services.gpg-agent = {
      enable = true;
      defaultCacheTtl = 1800;
      enableSshSupport = true;
      grabKeyboardAndMouse = true;
      enableScDaemon = true;
    };
  };
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
