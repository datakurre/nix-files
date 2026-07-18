{
  config,
  pkgs,
  lib,
  ...
}:
{
  imports = import ./modules/home/default.nix;
  home.file.".bashrc.d/99-nix.sh".text = ''
    . ${config.home.homeDirectory}/.nix-profile/etc/profile.d/nix.sh
    export SHELL=${config.home.homeDirectory}/.nix-profile/bin/nu
    export XTERM_SHELL=${config.home.homeDirectory}/.nix-profile/bin/nu
  '';
  home.file.".Xmodmap".source = lib.mkForce (
    builtins.toFile "Xmodmap" ''
      pointer = 1 2 3 4 5 6 7 0 9 10
    ''
  );
  xresources.properties."Xft.dpi" = "192";
  xsession.enable = true;
  xsession.initExtra = ''
    xrandr --dpi 192
  '';
}
