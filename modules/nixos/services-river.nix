{
  config,
  pkgs,
  lib,
  ...
}:
let
  waylandEnv = {
    MOZ_ENABLE_WAYLAND = "1";
    NIXOS_OZONE_WL = "1";
    GDK_SCALE = "2";
    QT_SCALE_FACTOR = "2";
  };
  envCmd = lib.concatStringsSep " " (
    [
      "env"
      "XDG_CURRENT_DESKTOP=river"
    ]
    ++ lib.mapAttrsToList (k: v: "${k}=${v}") waylandEnv
  );
in
{
  environment.sessionVariables = {
    _JAVA_AWT_WM_NONREPARENTING = "1";
    XCURSOR_THEME = "Adwaita";
    XCURSOR_SIZE = "48";
  }
  // waylandEnv;
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
  programs.dconf.enable = true;
  services = {
    blueman.enable = true;
    gnome.at-spi2-core.enable = true;
    gvfs.enable = true;
    udev.packages = [ pkgs.gnome-settings-daemon ];
    greetd = {
      enable = true;
      settings.default_session = {
        command = "${pkgs.tuigreet}/bin/tuigreet --time --remember --cmd '${envCmd} ${pkgs.river-classic}/bin/river'";
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
