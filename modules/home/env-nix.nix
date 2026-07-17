{ ... }:
{
  programs.nushell.environmentVariables = {
    NETRC = "/etc/nix/netrc";
    NIX_REMOTE = "daemon";
    SSL_CERT_FILE = "/etc/ssl/certs/ca-bundle.crt";
  };
}
