{
  nix = {
    settings.substituters = [
      "https://datakurre.cachix.org"
    ];
    settings.trusted-public-keys = [
      "robots-from-jupyter.cachix.org-1:181UTo4rIog6BLr9D4t79g5W8yLcf9+LDEaDZIN+dJw="
    ];
    settings.trusted-users = [ "root" "datakurre" "atsoukka" ];
  };
}
