{ pkgs, ... }:
{
  # Register the fonts below with fontconfig. Required on standalone
  # Home Manager (non-NixOS) hosts; harmless on NixOS.
  fonts.fontconfig.enable = true;
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
}
