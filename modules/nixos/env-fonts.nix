{ config, pkgs, ... }:
{
  fonts.fontconfig.enable = true;
  fonts.fontDir.enable = true;
  fonts.enableGhostscriptFonts = true;
}
