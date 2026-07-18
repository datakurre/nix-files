{ config, pkgs, ... }:
{
  documentation.nixos.enable = false;
  environment.systemPackages = [ pkgs.gnumake ];
  nix = {
    channel.enable = false;
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 180d";
    };
    settings = {
      auto-optimise-store = true;
      experimental-features = [
        "nix-command"
        "flakes"
      ];
      extra-sandbox-paths = [
        "/dev/urandom"
        "/etc/ssl/certs/ca-certificates.crt"
      ];
      keep-derivations = true;
      keep-outputs = true;
      max-free = 1024 * 1024 * 1024;
      min-free = 100 * 1024 * 1024;
      sandbox = true;
      substituters = [ "https://cache.nixos.org" ];
      trusted-users = [
        "root"
        "${config.user.name}"
      ];
    };
  };
}
