{
  config,
  pkgs,
  lib,
  ...
}:
{
  fonts.fontconfig.enable = true;
  home = {
    packages = [
      pkgs.cantarell-fonts
      pkgs.corefonts
      pkgs.dejavu_fonts
      pkgs.freefont_ttf
      pkgs.gentium
      pkgs.inconsolata
      pkgs.liberation_ttf
      pkgs.nerdfonts
      pkgs.powerline-fonts
      pkgs.terminus_font
      pkgs.ubuntu_font_family
    ];
  };
}
