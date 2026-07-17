{ config, pkgs, ... }:
{
  nix.settings = {
    substituters = [
      "https://datakurre.cachix.org"
      "https://nix-community.cachix.org"
    ];
    trusted-public-keys = [
      "datakurre.cachix.org-1:V0XJax7WX2ziQVVMOfsmVITq/pBqhkbFjjzBcyiTbVA="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };
}
