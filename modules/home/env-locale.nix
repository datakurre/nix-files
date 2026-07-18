{ ... }:
{
  home.file = {
    ".docutils.conf".source = ../env-locale-docutils.conf;
    ".editorconfig".source = ../env-locale-editorconfig.conf;
  };
  # Mirrors the NixOS-side xkb options (modules/nixos/env-locale.nix) so the
  # keyboard behaves the same on standalone Home Manager hosts.
  home.keyboard = {
    layout = "fi";
    options = [
      "eurosign:e"
      "caps:escape"
      "nbsp:none"
    ];
  };
  # Mirrors the NixOS-side locale for user sessions on non-NixOS hosts.
  home.language = {
    base = "fi_FI.UTF-8";
    address = "fi_FI.UTF-8";
    measurement = "fi_FI.UTF-8";
    monetary = "fi_FI.UTF-8";
    name = "fi_FI.UTF-8";
    numeric = "fi_FI.UTF-8";
    paper = "fi_FI.UTF-8";
    telephone = "fi_FI.UTF-8";
    time = "fi_FI.UTF-8";
  };
}
