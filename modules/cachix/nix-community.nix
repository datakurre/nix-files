{
  nix = {
    binaryCaches = [
      "https://datakurre.cachix.org"
    ];
    binaryCachePublicKeys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
    trustedUsers = [ "root" "datakurre" "atsoukka" ];
  };
}
