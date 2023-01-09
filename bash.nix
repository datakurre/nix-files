pkgs:
{
  enable = true;
  bashrcExtra = ''
    export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
    export NIX_PATH=$HOME/.nix-defexpr/channels$'' + ''{NIX_PATH:+:}$NIX_PATH
  '';
  sessionVariables = {
    LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
    EDITOR = "vim";
    NIX_REMOTE = "daemon";
    GS_OPTIONS = "-sPAPERSIZE=a4";
    SSL_CERT_FILE = "/etc/ssl/certs/ca-bundle.crt";
  };
}
