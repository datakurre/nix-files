{ config, pkgs, ... }:

{
  imports = [
    ./generated.nix
    ./manual.nix
  ];
}
