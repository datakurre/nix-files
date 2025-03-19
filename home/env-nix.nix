{ config, pkgs, ... }:
{
  documentation.nixos.enable = false;
  environment.systemPackages = [ pkgs.gnumake ];
  home-manager.users.${config.user.name} = {
    programs.nushell.environmentVariables = {
      NETRC = "/etc/nix/netrc"; # TODO...
      NIX_REMOTE = "daemon";
      SSL_CERT_FILE = "/etc/ssl/certs/ca-bundle.crt";
    };
  };
  nix = {
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 180d";
    };
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
        "/etc/ssl/certs/ca-certificates.crt"
      ];
      sandbox = true;
      substituters = [ "https://cache.nixos.org" ];
      trusted-users = [
        "root"
        "${config.user.name}"
      ];
    };
  };
}
