{
  config,
  pkgs,
  lib,
  ...
}:
{
  imports =
    (import ./modules/home/default.nix)
    ++ [
      ./modules/home/services-river.nix
      ./modules/home/services-xmonad.nix
    ];
  programs.home-manager.enable = true;
  # dconf D-Bus activation requires a running GNOME/dconf session; on RHEL 9
  # the system-level dconf service is not set up by Home Manager alone (only
  # NixOS does this via programs.dconf.enable).  Disable here to prevent
  # `home-manager switch` from failing outside a desktop session.
  dconf.enable = lib.mkForce false;
  home.sessionVariables.TMPDIR = "/home/atsoukka/MyTemp";
  xdg.configFile."nix/nix.conf".text = ''
    experimental-features = nix-command flakes
  '';
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
