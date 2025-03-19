{ config, pkgs, ... }:
{
  programs = {
    bash.bashrcExtra = ''
      export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
    '';
    nushell.envFile.text = ''
      ''$env.SSH_AUTH_SOCK = $"(gpgconf --list-dirs agent-ssh-socket)"
    '';
    ssh.enable = true;
    gpg.enable = true;
  };
  services = {
    gpg-agent.enable = true;
    gpg-agent.enableBashIntegration = true;
    gpg-agent.enableSshSupport = true;
    gpg-agent.enableScDaemon = true;
    gpg-agent.pinentryPackage = pkgs.pinentry-qt;
  };
  home.packages = [ pkgs.gnupg ];
}
