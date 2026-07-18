{ pkgs, ... }:
{
  # Session tools started via xsession.initExtra below (or referenced from
  # services-xmonad.hs). Listed here so they also exist on standalone
  # Home Manager hosts, where there is no NixOS environment.systemPackages.
  home.packages = [
    pkgs.brightnessctl
    pkgs.cbatticon
    pkgs.pasystray
    pkgs.xmousepasteblock
  ];
  services = {
    stalonetray = {
      config = {
        decorations = null;
        dockapp_mode = null;
        geometry = "4x1-0+0";
        max_geometry = "5x1-0+0";
        grow_gravity = "SW";
        icon_gravity = "SW";
        kludges = "force_icons_size";
        skip_taskbar = true;
        sticky = true;
      };
      enable = true;
    };
    screen-locker = {
      enable = true;
      lockCmd = "${pkgs.xlockmore}/bin/xlock -mode xjack -erasedelay 0";
    };
  };
  xresources.properties = {
    "Xcursor.theme" = "Adwaita";

    "XTerm*wideChars" = "true";
    "XTerm*locale" = "true";
    "XTerm*utf8" = "true";
    "XTerm*vt100Graphics" = "true";

    "XTerm*selectToClipboard" = "true";
    "XTerm*faceName" = "DejaVu Sans Mono for Powerline";
    "XTerm*faceSize" = "11";
    "XTerm*saveLines" = "1024";

    "*background" = "#002b36";
    "*foreground" = "#839496";
    "*fadeColor" = "#002b36";
    "*cursorColor" = "#93a1a1";
    "*pointerColorBackground" = "#586e75";
    "*pointerColorForeground" = "#586e75";

    # black dark/light
    "*color0" = "#073642";
    "*color8" = "#002b36";

    # red dark/light
    "*color1" = "#dc322f";
    "*color9" = "#cb4b16";

    # green dark/light
    "*color2" = "#859900";
    "*color10" = "#586e75";

    # yellow dark/light
    "*color3" = "#b58900";
    "*color11" = "#657b83";

    # blue dark/light
    "*color4" = "#268bd2";
    "*color12" = "#839496";

    # magenta dark/light
    "*color5" = "#d33682";
    "*color13" = "#6c71c4";

    # cyan dark/light
    "*color6" = "#2aa198";
    "*color14" = "#93a1a1";

    # white dark/light
    "*color7" = "#eee8d5";
    "*color15" = "#fdf6e3";
  };
  # The session is fully managed by Home Manager on both NixOS and
  # standalone hosts: NixOS only registers the "none+xmonad" session, which
  # executes ~/.xsession generated here. Tray applets not available as Home
  # Manager services are started in initExtra.
  xsession = {
    enable = true;
    initExtra = ''
      ${pkgs.xmousepasteblock}/bin/xmousepasteblock &
      ${pkgs.pasystray}/bin/pasystray &
      sleep 0.3 && ${pkgs.cbatticon}/bin/cbatticon &
    '';
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ../services-xmonad.hs;
    };
  };
}
