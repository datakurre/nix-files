{ pkgs, lib, ... }:
{
  home.packages = [
    pkgs.cantarell-fonts
    pkgs.corefonts
    pkgs.dejavu_fonts
    pkgs.freefont_ttf
    pkgs.gentium
    pkgs.inconsolata
    pkgs.liberation_ttf
    pkgs.powerline-fonts
    pkgs.terminus_font
  ];
  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [ "corefonts" ];
}
