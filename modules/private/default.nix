{ pkgs, lib, ... }:

let
  folder = ./. ;
  toImport = name: value: folder + ("/" + name);
  filterCaches = key: value: key != "default.nix" && value == "regular" && lib.hasSuffix ".nix" key;
  imports = lib.mapAttrsToList toImport (lib.filterAttrs filterCaches (builtins.readDir folder));
in {
  inherit imports;
}
