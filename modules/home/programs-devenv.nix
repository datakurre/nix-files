{ pkgs, ... }:
{
  home.packages = [
    pkgs.unstable.cachix
    pkgs.unstable.devenv
  ];
}
