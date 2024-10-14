{ config, pkgs, ... }:
{
  home-manager.users.${config.user.name} = {
    home = {
      packages = [ pkgs.cachix ];
    };
  };
  nix.settings = {
    substituters = [
      "https://datakurre.cachix.org"
      "https://nix-community.cachix.org"
      "https://robots-from-jupyter.cachix.org"
      "https://vasara-bpm.cachix.org"
    ];
    trusted-public-keys = [
      "datakurre.cachix.org-1:ayZJTy5BDd8K4PW9uc9LHV+WCsdi/fu1ETIYZMooK78="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "robots-from-jupyter.cachix.org-1:181UTo4rIog6BLr9D4t79g5W8yLcf9+LDEaDZIN+dJw="
      "vasara-bpm.cachix.org-1:T18iQZQvYDy/6VdGmttnkkq7rYi3JP0S1RjjdnXNu/4="
    ];
  };
}
