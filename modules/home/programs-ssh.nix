{
  pkgs,
  lib,
  osConfig ? null,
  ...
}:
let
  usePcsc = osConfig != null && (osConfig.services.pcscd.enable or false);
in
{
  programs = {
    bash.bashrcExtra = ''
      export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
    '';
    nushell.envFile.text = ''
      ''$env.SSH_AUTH_SOCK = $"(gpgconf --list-dirs agent-ssh-socket)"
    '';
    ssh = {
      enable = true;
      enableDefaultConfig = false;
      settings."*" = {
        ForwardAgent = false;
        AddKeysToAgent = "no";
        Compression = false;
        ServerAliveInterval = 0;
        ServerAliveCountMax = 3;
        HashKnownHosts = false;
        UserKnownHostsFile = "~/.ssh/known_hosts";
        ControlMaster = "no";
        ControlPath = "~/.ssh/master-%r@%n:%p";
        ControlPersist = "no";
      };
    };
  };
  home.packages = [ pkgs.gnupg ];

  # When pcscd is present (NixOS path), force scdaemon to use it.
  # On standalone Home Manager hosts pcscd is not managed here, so leave CCID enabled.
  home.file.".gnupg/scdaemon.conf" = lib.mkIf usePcsc {
    text = "disable-ccid\n";
  };

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 1800;
    enableSshSupport = true;
    grabKeyboardAndMouse = true;
    enableScDaemon = true;
    pinentry.package = pkgs.pinentry-qt;
  };
}
