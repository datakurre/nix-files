{ pkgs, ... }:
{
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
    pinentry.package = pkgs.pinentry-qt;
  };
}
