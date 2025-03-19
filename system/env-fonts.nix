{
  config,
  pkgs,
  lib,
  ...
}:
{
  fonts.fontconfig.enable = true;
  fonts.fontDir.enable = true;
  fonts.enableGhostscriptFonts = true;
  home-manager.users.${config.user.name} = {
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
  };
  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [ "corefonts" ];
}
