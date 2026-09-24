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
  # Home Manager adds the `headless` wlroots backend to the River session only
  # on Makondo, where OBS uses its virtual HEADLESS-1 presentation output.
  #
  # Deliberately NOT in environment.sessionVariables: WLR_* is read by every
  # wlroots compositor, and leaking it session-wide would give nested or
  # secondary compositors phantom outputs too.
  #
  # Makondo's kanshi profile includes HEADLESS-1 because profiles match the
  # whole connected output set. Other hosts should list only their active
  # outputs.
in
{
  options.services.river-headless-output.enable = lib.mkEnableOption "River's virtual presentation output";

  config = {
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
          command = "${pkgs.tuigreet}/bin/tuigreet --time --remember --cmd '${config.user.home}/.nix-profile/bin/river-session'";
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
  };
}
