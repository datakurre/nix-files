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

  # Route scdaemon through pcscd instead of GnuPG's internal CCID driver; the two cannot
  # both claim the YubiKey's CCID interface. Pairs with services.pcscd.enable in
  # modules/nixos/hw-yubikey.nix. Declared here rather than left as a hand-written
  # ~/.gnupg/scdaemon.conf so both machines stay in step.
  home.file.".gnupg/scdaemon.conf".text = "disable-ccid\n";

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 1800;
    enableSshSupport = true;
    grabKeyboardAndMouse = true;
    enableScDaemon = true;
    pinentry.package = pkgs.pinentry-qt;
  };
}
