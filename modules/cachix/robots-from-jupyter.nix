{
  nix = {
    binaryCaches = [
      "https://datakurre.cachix.org"
    ];
    binaryCachePublicKeys = [
      "robots-from-jupyter.cachix.org-1:181UTo4rIog6BLr9D4t79g5W8yLcf9+LDEaDZIN+dJw="
    ];
    trustedUsers = [ "root" "datakurre" "atsoukka" ];
  };
}
