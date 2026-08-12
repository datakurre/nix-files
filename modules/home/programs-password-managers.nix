{ pkgs, ... }:
{
  home.packages = with pkgs; [
    pass
    rbw
    bitwarden-cli
  ];
}
