{ pkgs, outlineEditor, ... }:
{
  home.packages = [ outlineEditor.packages.${pkgs.system}.default ];
}
