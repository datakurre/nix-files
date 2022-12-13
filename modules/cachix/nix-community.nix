{
  nix = {
    settings.substituters = [
      "https://datakurre.cachix.org"
    ];
    settings.trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
    settings.trusted-users = [ "root" "datakurre" "atsoukka" ];
  };
}
