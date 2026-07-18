{
  config,
  pkgs,
  lib,
  ...
}:
{
  environment.systemPackages = [
    pkgs.river-classic
    pkgs.xwayland
    pkgs.blueman
    pkgs.adwaita-icon-theme
    pkgs.nemo
    pkgs.nemo-fileroller
    pkgs.networkmanagerapplet
    pkgs.paprefs
    pkgs.pavucontrol
    pkgs.qpaeq
  ];
  environment.sessionVariables = {
    _JAVA_AWT_WM_NONREPARENTING = "1";
    GDK_SCALE = "2";
    MOZ_ENABLE_WAYLAND = "1";
    QT_SCALE_FACTOR = "2";
    XCURSOR_THEME = "Adwaita";
    XCURSOR_SIZE = "48";
  };
  programs.dconf.enable = true;
  services = {
    blueman.enable = true;
    gnome.at-spi2-core.enable = true;
    gvfs.enable = true;
    udev.packages = [ pkgs.gnome-settings-daemon ];
    greetd = {
      enable = true;
      settings.default_session = {
        command = "${pkgs.tuigreet}/bin/tuigreet --time --remember --cmd 'env XDG_CURRENT_DESKTOP=river ${pkgs.river-classic}/bin/river'";
        user = "greeter";
      };
    };
  };
  xdg.portal = {
    enable = true;
    extraPortals = [
      pkgs.xdg-desktop-portal-wlr
      pkgs.xdg-desktop-portal-gtk
    ];
    config.river = {
      default = [
        "wlr"
        "gtk"
      ];
    };
  };
}
