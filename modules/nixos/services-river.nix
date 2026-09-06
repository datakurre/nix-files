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
  };
  # wlroots backends for the login session. `headless` adds a virtual output
  # (HEADLESS-1) that river composites but never scans out. It is what OBS
  # captures while presenting: the panel keeps showing OBS and notes, while the
  # slides live on an output only the recording sees. Without it, screencopy can
  # only ever hand OBS the output you are looking at -- there is no way to
  # capture "another tag", because a tag that is not mapped is not rendered.
  #
  # Deliberately NOT in environment.sessionVariables: WLR_* is read by every
  # wlroots compositor, and leaking it session-wide would give nested or
  # secondary compositors phantom outputs too.
  #
  # Every kanshi profile must now also list HEADLESS-1. kanshi applies a profile
  # only when it matches the *whole* connected output set, so a profile naming
  # eDP-1 alone silently stops matching once this backend is on -- and a
  # non-matching profile leaves the panel at scale 1, which is exactly the
  # "GTK/Qt clients render tiny" failure documented in machines/*/manual.nix.
  compositorEnv = {
    WLR_BACKENDS = "libinput,drm,headless";
    WLR_HEADLESS_OUTPUTS = "1";
  };
  envCmd = lib.concatStringsSep " " (
    [
      "env"
      "XDG_CURRENT_DESKTOP=river"
    ]
    ++ lib.mapAttrsToList (k: v: "${k}=${v}") (waylandEnv // compositorEnv)
  );
in
{
  environment.sessionVariables = {
    _JAVA_AWT_WM_NONREPARENTING = "1";
    XCURSOR_THEME = "Adwaita";
    XCURSOR_SIZE = "24";
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
    pkgs.alsa-utils
    pkgs.brightnessctl
    pkgs.playerctl
  ];
  programs.dconf.enable = true;
  services = {
    blueman.enable = true;
    gnome.at-spi2-core.enable = true;
    gvfs.enable = true;
    udev.packages = [
      pkgs.gnome-settings-daemon
      pkgs.brightnessctl
    ];
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
    config = {
      common.default = [
        "wlr"
        "gtk"
      ];
      river.default = [
        "wlr"
        "gtk"
      ];
    };
  };
}
