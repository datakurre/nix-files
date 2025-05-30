{ config, pkgs, ... }:
{
  programs.nushell.environmentVariables = {
    SSL_CERT_FILE = "/etc/ssl/certs/ca-bundle.crt";
  };
  nix = {
    package = pkgs.nix;
    extraOptions = ''
      auto-optimise-store = false
      builders-use-substitutes = true
      keep-outputs = true
      keep-derivations = true
      min-free = ${toString (100 * 1024 * 1024)}
      max-free = ${toString (1024 * 1024 * 1024)}
      experimental-features = nix-command flakes
    '';
    settings = {
      extra-sandbox-paths = [
        "/dev/urandom"
        "/etc/ssl/certs/ca-bundle.crt"
        "/etc/pki/ca-trust/extracted/pem/tls-ca-bundle.pem"
      ];
      sandbox = true;
      substituters = [ "https://cache.nixos.org" ];
      trusted-users = [
        "${config.home.username}"
      ];
    };
  };
}
